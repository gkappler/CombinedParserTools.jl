export html


export url
"""
    url(; blacklist=re"[^\\v\\h]", scheme = !!re"[a-z]+", token_parse = !!Repeat(AnyChar()), host_parse = join(token_parse,'.'), path_parse = token_parse, #path_file(),  query_parse = token_parse )

Create parser for generic URL syntax, see [Wikipedia](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Generic_syntax).
"""
function url(;
             blacklist=re"[^\v\h]",
             scheme = !!re"[a-z]+",
             token_char  = AnyChar(),
             token_parse = !!Repeat(token_char),
             host_parse = !!join(token_parse,'.'),
             path_parse = !!token_parse, #path_file(),
             query_parse = !!token_parse
             )
        Sequence(:scheme => Atomic(scheme),
                 ':', Atomic(Either(
                     "//",
                     PositiveLookbehind(caseless("mailto:")))),
                 :authority => Sequence(
                     :userinfo => Optional(Sequence(1,!!re"\w+", '@')),
                     :host => map(host_parse, !(Repeat1(blacklist .& CharNotIn("/:")))),
                     :port => (':',Numeric(Int64))[2]  | 80
                 ),
                 :path => Optional(Sequence(
                     2, '/', map(path_parse, !Repeat(blacklist .& CharNotIn("?#"))))),
                 :query => Optional(
                     Sequence(2,'?',   !!(Repeat(blacklist .& CharNotIn("#"))))), #   ∘ query_parse )),
                 :fragment => Optional(Sequence(2, '#', !!Repeat(blacklist))),
                 NegativeLookahead(CharIn(",!?.)]:")) ## not the best solution
                 )
end

function Base.show(io::IO, x::NamedTuple{fieldnames(result_type(url()))})
    _prefix(p,x...) = ( isempty(x) || x == tuple("") ) ? tuple() : (p,x...)
    _suffix(x,p) = x == "" ? "" : x*p
    printstyled(io, x.scheme,"://", color=:yellow)
    printstyled(io, _suffix(x.authority.userinfo,"@")..., color=:red)
    printstyled(io, x.authority.host..., color=:underline)
    printstyled(io, x.authority.port == 80 ? "" : x.authority.port)
    printstyled(io, "/", x.path..., color=:bold)
    printstyled(io, _prefix("?", x.query, _prefix("#", x.fragment...)...)..., color=:light_black)
end

@auto_hash_equals struct Node{A,T} <: AbstractToken
    name::Symbol
    attributes::Vector{A}
    children::Vector{T}
    function Node(name::Symbol, attrs, value)
        new{eltype(attrs),eltype(value)}(name, attrs,value)
    end
    function Node(name::AbstractString, attrs::Vector{A}, value::Vector{T}) where {A,T}
        new{A,T}(Symbol(name), attrs,value)
    end
    function Node{T}(name::AbstractString, attrs, value) where T
        new{Token,T}(Symbol(name), attrs, _convert(Vector{T},value))
    end
    function Node{A,T}(name, attrs, value) where {A,T}
        new{A,T}(Symbol(name), convert(Vector{A},attrs), convert(Vector{T},value))
    end
end

function Base.show(io::IO, x::Node) where {T}
    print(io,"<$(x.name)")
    for a in x.attributes
        if a isa Token
            print(io," ", variable(a), "=\"", value(a), "\"")
        else ## this is for templates and such in wikitext
            print(io,a)
        end
    end
    if !isempty(x.children)        
        print(io,">")
        for c in x.children
            print(io,c)
        end
        print(io,"</$(x.name)>")
    else
        print(io,"/>")
    end
end

# regex_neg_lookahead(e,match=AnyChar()) =
#     !Repeat(Sequence(NegativeLookahead(e),match))

export attribute_parser
"""
Parser matching an html attribute.
Key is a `word` parser, value is either a
- quoted string (escape ")
- a `numeric%` value (without quotes)
- a `numeric` value (without quotes)
- a `word` value (without quotes)
- a color `#xxxxxx` value (without quotes)

`result_type` is a [`Token`](@ref).
"""
attribute_parser =
    with_name(
        :attribute,
        Sequence(
            word, whitespace_maybe,"=", whitespace_maybe,
            Either(
                Sequence(
                    2, "\"",
                    !!Repeat(Sequence(2,NegativeLookahead("\""),Either(tuple("\\\"",AnyChar())))),
                    "\""),
                !!re"[0-9]+%",
                !!re"[-+]?[0-9]+",
                !!word,
                !!re"#[0-9A-Fa-f]{6}")
        ) do v
        Token(lowercase(v[1]), intern(v[5]))
        end
    )

attributes = join(attribute_parser, whitespace_horizontal)

"""
    html(tags=word, inner=AnyChar(), attrs=attributes)

`result_type` is a [`Node`](@ref)`{eltype(result_type(attrs)),result_type(inner)}`.
"""
function html(tags=word, inner=AnyChar(), attrs=attributes)
    html(result_type(inner), tags, attrs) do until
        Repeat_until(inner, until)
    end
end


"""
    html(inner::Function, T::Type, tags=word, attrs_parser=attributes)

`result_type` is a [`Node`](@ref)`{eltype(result_type(attrs)),T}`.
"""
function html(inner::Function, T::Type, tags=word, attrs_parser=attributes)
    A = eltype(result_type(attrs_parser))
    function r(x,)
        (tag,attrs) = x
        Either(map( # Node{A,T},
                    (v) -> Node(tag, attrs, T[]),
                    parser("/>")),
               Sequence( # Node{A,T},
                         ">",
                         inner(Sequence("</",caseless(tag),">"))) do v
               Node(tag, attrs, v[2])
               end)
    end
    FlatMap{Node{A,T}}(
        r,
        Sequence(
            2,
            "<",
            Sequence(
                !tags,
                Optional(
                    Sequence(
                        2,
                        whitespace_maybe,
                        attrs_parser,
                        whitespace_maybe)))))
end

