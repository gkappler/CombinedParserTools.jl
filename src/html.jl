


export attribute_parser
attribute_parser =
    map(
        (v) -> Token(lowercase(v[1]), intern(v[5])),
        Token,
        seq(
            word, whitespace_maybe,"=", whitespace_maybe,
            Either(
                Sequence(
                    2, "\"",
                    regex_neg_lookahead("\"",re"(?:.|\\\")"),"\""),
                re"[0-9]+%",
                re"[-+]?[0-9]+",
                word,
                re"#[0-9A-Fa-f]{6}")
        ))

attributes = alternate(attribute_parser, whitespace_horizontal)

function html(tags=word, inner=AnyChar(), attrs=attributes)
    html(result_type(inner), tags, attrs) do until
        Repeat_until(inner, until)
    end
end

function html(inner::Function, T::Type, tags=word, attrs_parser=attributes)
    A = eltype(result_type(attrs_parser))
    function r(x,)
        (tag,attrs) = x
        Either(map(
            #Node{A,T},
            (v) -> Node(tag, attrs, T[]),
            parser("/>")),
               Sequence(
                   # Node{A,T},
                   ">",
                   inner(Sequence("</",tag,">"))) do v
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
