

export tokenstring,default_token,quotes
tokenstring(unknown=AnyChar()) = Repeat(default_token(unknown) | !!unknown)

export Line, Paragraph, Body
export LinePrefix

export TokenString
const TokenString = Vector{<:AbstractToken}
import Base: convert
Base.convert(::Type{TokenString}, x::String) = tokenize(x)

function Base.string(x::TokenString)
    b = IOBuffer()
    bc = IOContext(b, :compact => true)
    for e in x
        show(bc, e)
    end
    String(take!(b))
end
# @deprecate TokenString(x...) tokenize(x...)

# const TokenTuple = Tuple{Vararg{Token, N} where N}
# TokenString{Tt, T} = Tuple{Vararg{Token{t,T} where {t <:Tt}, N} where N}
# TokenNest{Tv} = Tuple{Vararg{Union{TokenTuple{t},Token{t,s}} where {t, s <:Tv}, N} where N}
## TokenTuple(x::AbstractToken{Any, Tv}...) where {Tv} = x
# TokenString(x::TokenTuple) = x

export tokens
tokens(x::Vector{<:Union{AbstractToken, AbstractString, Symbol}}) =
    Iterators.repeated(1  => x, 1)
tokens(x::Union{Number,Symbol, AbstractString, AbstractToken}) =
    Iterators.repeated(1 => Iterators.repeated(x, 1), 1)
## tokens(d::Dict{Symbol,<:AbstractString}) =
##     [ 1 => [ Token(x.first, x.second) for x in d ] ]
tokens(d::Dict) =
    tuple() #Iterators.flatten( tokens(x.second) for x in d )
tokens(x::Vector) =
    Iterators.flatten(tokens(y) for y in x)

"""
    tokens(n::NamedTuple{names,t})

Recursively collect all tokens in a NamedTuple.
"""
function tokens(n::NamedTuple{names,t}) where {names,t}    
    val(field) = tokens(getproperty(n,field))
    R = Iterators.flatten( val(field)
                           for field in names
                           # if !isempty(val(field))
                           )
    R
    # Pair[ w => collect(v) for (w,v) in R ]    
end

"""
    tokens(n::T)

Recursively collect all tokens in a struct.
"""
function tokens(n::T) where {T}
    val(field) = tokens(getfield(n,field))
    R = Iterators.flatten( val(field)
                           for field in fieldnames(T)
                           # if !isempty(val(field))
                           )
    R
    # Pair[ w => collect(v) for (w,v) in R ]    
end



"""
Wrapper class for a prefix vector.
Wrapping used for interning.
"""
@auto_hash_equals struct LinePrefix{I}
    prefix::Vector{I}
end
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
Base.convert(::Type{Vector{I}}, x::LinePrefix) where {I} =
    convert(Vector{I}, x.prefix)
Base.convert(::Type{LinePrefix{J}}, x::Vector{I}) where {I,J} =
    LinePrefix{J}(convert(Vector{J}, x))
Base.pushfirst!(v::LinePrefix, x)  =
    pushfirst!(v.prefix,x)
Base.push!(v::LinePrefix, x)  =
    push!(v.prefix,x)

export Line

@auto_hash_equals struct Line{I,T}
    prefix::LinePrefix{I}
    tokens::Vector{T}
    function Line{I,T}(prefix::Vector, t::Vector, newline::AbstractString)::Line{I,T} where {I,T}
        new{I,T}( prefix,
                   newline !="" ? vcat(t, Token(:whitespace, newline)) : t)::Line{I,T}
    end
    function Line{I,T}(prefix::Vector, t::Vector, newline::AtEnd)::Line{I,T} where {I,T}
        new{I,T}( prefix, t)::Line{I,T}
    end
    Line{I}(t::Vector{T}) where {I,T} =
        new{I,T}(I[],t)
    Line{I,T}(t::Vector{<:T}) where {I,T} =
        new{I,T}(I[],convert(Vector{T}, t))
    function Line{I,T}(prefix::Vector, t::Vector) where {I,T}
        new{I,T}(LinePrefix{I}(prefix), t)
    end
    function Line(prefix::Vector{I}, t::Vector{T}) where {I,T}
        new{I,T}(LinePrefix{I}(prefix), t)
    end
end
Line(t::Vector{T}) where {T} =
    Line{NamedString}(t)
# function Line(prefix::NTuple{N,NamedString}, t::Vector{T}) where {N,T}
#     Line{NamedString,T}(
#         LinePrefix{NamedString}(NamedString[prefix...]),
#         t)
# end

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
    ValueIn{T}("heading") do x
        !isempty(x.prefix) &&
            variable(x.prefix[end])==:headline &&
            f(x.prefix[end])
    end

is_type(T::Type) =
    ValueIn{T}(string(t))

"""
is not a headline
"""
is_line(T::Type{<:Line}) =
    ValueIn{T}("Line") do x
        ( isempty(x.prefix) ||
          variable(x.prefix[end])!=:headline)
    end

is_line() =
    is_line(Line{NamedString,LineContent})

