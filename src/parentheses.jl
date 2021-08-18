export Node
export TokenPair
export open_close
parentheses = Dict{Any,Any}(:paren=>("(", ")"),
                            :bracket=>("[", "]"),
                            :curly=>("{", "}"),
                            :angle=>("<", ">"),
                            :quote=>("\"","\""),
                            :squote=> ("'","'"),
                            :german_quote => ("„","“"),
                            :htmlcomment=> ("<!--","-->"),
                            ## :pre=> ("<pre>","</pre>"),
                            ## :nowiki=> ("<nowiki>","</nowiki>"),
                            )

import Base: with_output_color
@auto_hash_equals struct TokenPair{K,V} <: AbstractToken
    key::K
    value::V
end

"""
    open_close(name::Symbol, inner, open, close=open, stops::Tuple=tuple())

Parser returning a [`TokenPair`](@ref)`(name,inner_parsing)`
"""
function open_close(name::Symbol, inner, open, close=open, stops::CombinedParser = Never()) # remove!
    with_name(
        name,
        Sequence(open,
                 Repeat_until(
                     inner,
                     Either(close, stops; simplify=true))) do v
        TokenPair(name,v[2])
        end)
end

@deprecate open_close(name::Symbol, inner, open, close, stops::Tuple; kw...) open_close(name, inner, open, Either(close, stops...; simplify=true); kw...)

function Base.show(io::IO, z::TokenPair)
    inner_print(io::IO,x::AbstractVector) =
        for t in x; print(io, t); end
    inner_print(io::IO,x) =
        print(io, x)
    
    if z.key==:hyphenation
        join(io, z.value, "·")
    elseif z.key==:link
        join(io, z.value)
    elseif z.key==:italics
        with_output_color(inner_print, :underline, io, z.value)
    elseif z.key==:bold
        with_output_color(inner_print, :bold, io, z.value)
    elseif z.key==:bolditalics
        with_output_color(inner_print, :bold, io, z.value)
    else
        open, close = get(parentheses, z.key, (z.key,z.key))
        print(io, open)
        inner_print(io,z.value)
        print(io, close)
    end
end

