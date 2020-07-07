export Line, Paragraph, Body
using AutoHashEquals
export LinePrefix
"""
Wrapper class for a prefix vector.
Wrapping used for interning.
"""
struct LinePrefix{I}
    prefix::Vector{I}
end
==(x::LinePrefix,y::LinePrefix) =
    x.prefix == y.prefix
Base.hash(x::LinePrefix,h::UInt) =
    hash(x.prefix,h)
Base.lastindex(x::LinePrefix) =
    lastindex(x.prefix)
Base.length(x::LinePrefix) =
    length(x.prefix)
Base.isempty(x::LinePrefix) =
    isempty(x.prefix)
Base.iterate(x::LinePrefix, a...) =
    iterate(x.prefix, a...)
Base.getindex(x::LinePrefix, a...) =
    getindex(x.prefix, a...)
Base.convert(::Type{Vector{I}}, x::LinePrefix{J}) where {I,J} =
    convert(Vector{I}, x.prefix)
Base.convert(::Type{LinePrefix{J}}, x::Vector{I}) where {I,J} =
    LinePrefix{J}(convert(Vector{J}, x))
Base.pushfirst!(v::LinePrefix, x) where {J} =
    pushfirst!(v.prefix,x)
Base.push!(v::LinePrefix, x) where {J} =
    push!(v.prefix,x)


@auto_hash_equals struct Line{I,T}
    prefix::LinePrefix{I}
    tokens::Vector{T}
end
Line(t::Vector{T}) where {T} =
    Line{NamedString}(t)
Line{I}(t::Vector{T}) where {I,T} =
    Line(I[],t)
function Line(prefix::Vector{I}, t::Vector{T}) where {I,T}
    Line{I,T}(LinePrefix{I}(prefix), t)
end
function Line(prefix::Vector{I}, t::Vector{T}, newline::AbstractString) where {I,T}
    Line( prefix,
          vcat(t, Token(:whitespace, newline)))
end
function Line(prefix::NTuple{N,NamedString}, t::Vector{T}) where {N,T}
    Line{NamedString,T}(
        LinePrefix{NamedString}(NamedString[prefix...]),
        t)
end
emptyLine(x::Vararg{T}) where T =
    Line{T,T}([ Token(:whitespace,"") ],
              T[x...])

import Base: convert
Base.convert(::Type{Line{I,T}}, x::Line{J,S}) where {I,J,S,T} =
    Line(convert(Vector{I}, x.prefix), convert(Vector{T}, x.tokens))
Base.convert(::Type{Line{I,T}}, x::Vector) where {I,T} =
    Line(I[], convert(Vector{T}, x))

function Base.show(io::IO, i::Line{I,T}) where {I,T}
    if !isempty(i.prefix) && variable(i.prefix[1]) == :headline
        level = parse(Int, value(i.prefix[1]))
        wikihead = repeat("=", level)
        print(io, wikihead, " ")
        tail = Token[]
        for x in i.tokens
            if !isequal(x, Token(:whitespace,"\n"))
                print(io, x)
            else
                push!(tail,x)
            end
        end
        print(io, wikihead)
        for x in tail
            print(io, x)
        end 
    else
        for x in i.prefix
            print(io, x.value === missing ? "" : x)
        end
        for x in i.tokens
            print(io, x)
        end
    end
end


Paragraph{I,T} = Vector{Line{I,T}}
Paragraph(x::Paragraph) = x
## Base.show(io::IO, v::Type{Paragraph{T}}) where T = print(io, "Paragraph{$T}")
Base.show(io::IO, v::AbstractVector{<:Line}) =
    for x in v
        print(io,x)
    end

Body{I,T} = Vector{Paragraph{I,T}}
## Base.show(io::IO, v::Type{Body{T}}) where T = print(io, "Body{$T}")
Base.show(io::IO, v::Body) =
    for x in v
        print(io,x)
    end
# Base.show(io::IO, m::MIME"text/markdown", x::Token) =
#     if x.name in [ :literal, :delimiter, :whitespace ]
#         print(io, x.value)
#     else
#         print(io,"""[$(x.value)]($(x.name) "$(x.name)")""")
#     end
# Base.show(io::IO, m::MIME"text/markdown", x::Line) = println(io,m,x.indent,x.tokens...)
# Base.show(io::IO, m::MIME"text/markdown", x::Vector{Line}) = println(io,m,x...)
# Base.show(io::IO, m::MIME"text/markdown", x::Vector{Vector{Line}}) = println(io,m,x...)

export is_type, is_heading, is_template, is_template_line, is_line, LineContent
LineContent = AbstractToken

is_heading(f=x->true, T = Line{NamedString,LineContent}) =
    IteratorParser(T,"heading") do x
        !isempty(x.prefix) &&
            variable(x.prefix[end])==:headline &&
            f(x.prefix[end])
    end
is_type(t::Type) =
    IteratorParser(t, string(t))

"""
is not a headline
"""
is_line(t::Type{<:Line}) =
    IteratorParser(t,"Line") do x
        ( isempty(x.prefix) ||
          variable(x.prefix[end])!=:headline)
    end
is_line() = is_line(Line{NamedString,LineContent})

