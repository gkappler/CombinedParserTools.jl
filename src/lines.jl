


export Line, Paragraph, Body

export LinePrefix
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


struct Line{I,T}
    prefix::LinePrefix{I}
    tokens::Vector{T}
end

# BasePiracy.construct(::Type{Line{I,T}};prefix=I[],tokens=T[]) where {I,T} =
#     Line{I,T}(prefix,tokens)

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
==(a::Line, b::Line) = a.prefix==b.prefix && a.tokens== b.tokens
hash(x::Line, h::UInt) = hash(x.prefix, hash(x.tokens))
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

emptyLine(x::Vararg{T}) where T = Line{T,T}([ Token(:whitespace,"") ],
                                   T[x...])


export print_org_tree
print_org_tree(x) = print_org_tree(stdout,x)

print_org_tree(io::IO, x) =
    print(io,x)
print_org_tree(io::IO, x::AbstractToken) =
    x != Token(:whitespace, "\n") && print(io,x)
function print_org_tree(io::IO, tree::Vector)
    for b in tree
        print_org_tree(io, b)
    end
end
function print_org_tree(io::IO, b::Pair)
    l, inner = b
    print_org_tree(io::IO, NamedString(l), inner)
end
function print_org_tree(io::IO, b::NamedString{:name}, inner)
    print(io, "#+name: ", value(l), "\n")
    print_org_tree(io, inner)
end
function print_org_tree(io::IO, b::NamedString{:orgblock}, inner)
    print(io, "#+begin_", value(l), "\n")
    print_org_tree(io, inner)
    print(io,  "#+end_", value(l), "\n")
end
function print_org_tree(io::IO, b::NamedString{:orgdrawer}, inner)
    print(io, ":", value(l), ":\n")
    print_org_tree(io, inner)
    print(io, ":END:\n" )
end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print(io, ":", value(l), ":\n")
#     print_org_tree(io, inner)
# end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print_org_tree(io, inner)
# end



