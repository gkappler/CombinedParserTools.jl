export AbstractToken, variable, value
export isinformative, isvariable

export NamedString
export Token
export @l_str, @ws_str, @delim_str, @T_str
export @annotate

export path_file

"""
An abstract type for annotated textual tokens.
"""
abstract type AbstractToken end
## TODO: rename variable to annotation
variable(x::AbstractToken) = error("implement variable(x::$(typeof(x)))")
value(x::AbstractToken) = error("implement value(x::$(typeof(x)))")
label(x::AbstractToken) = error("implement label(x::$(typeof(x)))")
isinformative(i) = true
isvariable(i) = false
isinformative(i::AbstractToken)  =
    !(variable(i) in [ :delimiter, :indent, :list, :enum, :whitespace ])
isvariable(i::AbstractToken)  =
    !(variable(i) in [ :literal, :capitalized ]) && isinformative(i)

Base.isless(x::AbstractToken, y::AbstractToken) =
    isless(value(x), value(y))


variable_colors=Dict(
    :ext => 36,
    :macro => 36,
    :number => 36,
    :root => :dark_gray,
    :folder => :blue,
    :file => :yellow,
    :syllable => :yellow,
    :operator => :yellow,
    :name => :yellow,
    :footnote => :yellow,
    :type => :red,
    :field => :light_red,
    :ellipsis => :grey,
    :whitespace => :grey,
    :list => :light_black,
    :protocol => :light_blue,
    :domain => :light_blue,
    :Symbol => :yellow,
    :String => :yellow,
    :paren => :light_black,
    :quote => :light_black,
    Symbol("wikt:de") => :light_blue,
    :htmlcomment => :light_black,
    :unknown => :light_red,
    :meaning => :light_black
)

function Base.show(io::IO, z::AbstractToken)
    if get(stdout,:color,false)
    color=get(variable_colors,
              Symbol(variable(z)), 36)
    if variable(z) in [ :literal, :capitalized, :delimiter ]
        value(z)!==missing && print(io,value(z))
    elseif !isinformative(z)
        printstyled(io, value(z); bold=true,
                    color=:darkgray)
    elseif value(z)===missing ## || value(z)==""
        printstyled(io, variable(z); bold=true,
                    color=color)
    else
        col=get(variable_colors, variable(z), missing)
        if variable(z) == :macro
            printstyled(
                io, "{{{", value(z),"}}}";
                bold=true, color=color
            )
        elseif col === missing
            if get(io,:compact,false)
                print(io,value(z))
            else
                printstyled(
                    io, "[[",variable(z), "][", value(z),"]]";
                    bold=true, color=color
                )
            end
        else
            printstyled(io, value(z); color=color)
        end
    end
    else
        print(io,value(z))
    end
end

############################################################
## Tokens
## TODO: move intenring into parsing (creating from a db interning in tokens wastes mem)

"parametrized Token struct -- dangerously slow!"
@auto_hash_equals struct NamedString{name} <: AbstractToken
    value::String
    function NamedString(name, value)
        new{Symbol(name)}(value)
    end
end
NamedString(x::NamedString) = x
variable(x::NamedString{name}) where name = name
value(x::NamedString) = getfield(x,1)

Base.show(io::IO,x::NamedString{:type}) =
    print(io,Token(:type,x.value),".")

Base.show(io::IO,x::NamedString{:field}) =
    print(io,Token(:field,x.value),"=")

Base.show(io::IO,x::NamedString{:whitespace}) =
    printstyled(io,x.value; color=:underline)

Base.propertynames(x::NamedString) = (:name,:value)
Base.getproperty(x::NamedString, p::Symbol) =
    if p == :name
        variable(x)
    elseif p == :value
        value(x)
    else
        error("no field $p in NamedString")
    end

"""
Represents an annotation of field `value::String` with field `name::Symbol`.
"""
@auto_hash_equals struct Token <: AbstractToken
    name::Symbol ## todo: CategoricalArrays.CategoricalValue
    value::String
    function Token(name::Symbol, value::T) where {T<:AbstractString}
        new(name, value)
    end
end
Token(name::Symbol, value::Union{Missing, Nothing}) = Token(name, "")
Token(name::Symbol) = Token(name, "")
Token(x::Pair) = Token(x.first, x.second)
Token(x::Pair{<:AbstractString,Symbol}) = Token(x.second, x.first)
Token(x::Token) = x

"""
    Token(p::NamedParser)

Parser matching p.parser, transforming the result in a
Token
"""
function Token(p::NamedParser)
    @assert result_type(p)<:AbstractString
    annotated(s::AbstractString) =
        Token(p.name,s)
    map(annotated, p)
end
function Token(name::AbstractString, value)
    Token(Symbol(name), value)
end


function NamedString(x::Token)
    NamedString(variable(x),value(x))
end
import Base: convert
Base.convert(::Type{Token},e::Pair) =
    Token(Symbol(e.first), e.second)
Base.convert(::Type{NamedString}, x::Token) =
    NamedString(variable(x),value(x))
Base.convert(::Type{Token}, x::NamedString) =
    Token(variable(x),value(x))



"""
    T_str(value, name)

String macro to create a Token.
```jldoctest
julia> T"Jack"name
Jack

julia> T"123"number
123
```
"""
macro T_str(value, name)
    Token(name, string(value))
end
macro l_str(x)
    Token(:literal, x)
end
macro ws_str(x)
    Token(:whitespace, x)
end
macro delim_str(x)
    Token(:delimiter, x)
end
# ws(x) = Token(:whitespace, x)


"""
```jldoctest
julia> digit = re"[0-9]";

julia> @annotate [digit, :lletter => re"[a-z]"]
|🗄... Either |> map(NamedString)
├─ [0-9] CharIn |> map(#161) |> with_name(:digit)
└─ [a-z] CharIn |> map(#161) |> with_name(:lletter)
::NamedString

```
"""
macro annotate(x)
    if x.head == :vect
        top = Expr(:vect)
        for e_ in x.args
            e = e_
            while e isa Expr && e.head==:call && e.args[1]==Symbol("!")
                e = e.args[2]
            end
            if e isa Symbol
                push!(top.args,:($(QuoteNode(e)) => $e_))
            elseif e.head==:call && e.args[1] == Symbol("=>")
                push!(top.args,e)
            else
                dump(x)
                error("@annotate supports only variables and `:annotation => parser`, not `$e`.")
            end
        end
        esc(:(Either( ( Token(parser(p)) for p in $top)...)))
    else
        dump(x)
        :()
    end
end


function path_file(path="/", blacklist=CharNotIn(path), sep=re"[ _\.-]")
    path_char=blacklist .& CharNotIn(path) .& CharNotIn(sep)
    path_parser = !!Repeat1(path_char)
    septoken = instance(Token,!!sep,:whitespace)
    Sequence(
        Optional(path => Token(:root,path), default=Token(:root,".$path")),
        with_name(:folder,Repeat(with_name(:folder,Sequence(Atomic(Repeat(Either(instance(Token, path_parser, :folder), septoken))),
                                                            instance(Token,path,:folder)) do v
                         push!(v[1],v[2])
                         v[1]
                         end))),
        Lazy(Repeat(with_name(:file,Either(instance(Token, !!Repeat1(path_char), :file), septoken)))),
        with_name(:ext,instance(Token, Optional(!re"\.[^.]+"), :ext)),
        AtEnd()) do v
            r = Token[v[1],Iterators.flatten(v[2])...,v[3]...]
            value(v[4])!="" && push!(r,v[4])
            r
        end
end





