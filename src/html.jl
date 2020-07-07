export html

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
                        whitespace_maybe)))),
        r)
end

