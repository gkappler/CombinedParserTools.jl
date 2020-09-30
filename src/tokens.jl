"""
Experimental module providing several types to store annotated text [`Token`](ref)s,
[`Line`](ref)s.
"""
module Tokens
############################################################
## Tokens
## TODO: move intenring into parsing (creating from a db interning in tokens wastes mem)
export AbstractToken, variable, value
export Token, TokenValue, TokenTuple, TokenString
import Base: ==, hash
using CombinedParsers
using CombinedParsers.Regexp
import CombinedParsers.Regexp: whitespace_maybe, whitespace_horizontal, word
using ..CombinedParserTools
import ..CombinedParserTools: footnote, quotes, delimiter, extension

"""
An abstract type for annotated textual tokens.
"""
abstract type AbstractToken end
## TODO: rename variable to annotation
variable(x::AbstractToken) = error("implement variable(x::$(typeof(x)))")
value(x::AbstractToken) = error("implement value(x::$(typeof(x)))")
label(x::AbstractToken) = error("implement label(x::$(typeof(x)))")


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

# export value_empty
# #value_empty(x::Pair) = value_empty(x.second) ## needed in tryparsenext
# value_empty(x::Vector) = isempty(x)
# value_empty(x) = false
# value_empty(::Union{Nothing,Missing}) = true
# value_empty(x::String) = x==""
# value_empty(x::AbstractToken) = value(x) === missing || value(x)==""

export isinformative, isvariable
isinformative(i) = true
isvariable(i) = false

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
            printstyled(
                io, "[[",variable(z), "][", value(z),"]]";
                bold=true, color=color
            )
        else
            printstyled(io, value(z); color=color)
        end
    end
    else
        print(io,value(z))
    end
end

             



export Token
"""
Represents an annotation of field `value::String` with field `name::Symbol`.
"""
struct Token <: AbstractToken
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
    map(v->Token(p.name,v),p)
end
function Token(name::AbstractString, value)
    Token(Symbol(name), value)
end

isinformative(i::Token)  =
    !(variable(i) in [ :delimiter, :indent, :list, :enum, :whitespace ])
isvariable(i::Token)  =
    !(variable(i) in [ :literal, :capitalized ]) && isinformative(i)


import Base: convert
Base.convert(::Type{Token},e::Pair) =
    Token(Symbol(e.first), e.second)

export @l_str, @ws_str, @delim_str, @T_str
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


export TokenPair
"""

"""
struct TokenPair{K,V} <: AbstractToken
    key::K
    value::V
end
==(x::TokenPair,y::TokenPair) =
    x.key==y.key && x.value==y.value
hash(x::TokenPair, h::UInt) = hash(x.key, hash(x.value,h))
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




export @annotate
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
        esc(:(Either( tuple(( Token(parser(p)) for p in $top)...))))
    else
        dump(x)
        :()
    end
end


export NamedString
"parametrized Token struct -- dangerously slow!"
struct NamedString{name} <: AbstractToken
    value::String
    function NamedString(name, value)
        new{Symbol(name)}(value)
    end
    function NamedString(x::Token)
        new{variable(x)}(value(x))
    end
end
function NamedString(x::NamedString)
    x
end
Base.convert(::Type{NamedString}, x::Token) =
    NamedString(variable(x),value(x))

Base.isless(x::NamedString, y::NamedString) =
    isless(value(x), value(y))

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

variable(x::NamedString{name}) where name = name
value(x::NamedString) = getfield(x,1)
==(x::NamedString,y::NamedString) =
    variable(x)==variable(y) && value(x)==value(y)
hash(x::NamedString, h::UInt) = hash(variable(x), hash(value(x),h))

export Node
struct Node{A,T} <: AbstractToken
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


==(a::Node, b::Node) = a.name==b.name && a.attributes==b.attributes && a.children==b.children
hash(x::Node, h::UInt) = hash(x.name, hash(x.attributes, hash(x.children,h)))
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

import InternedStrings: intern
# import ..ParserAlchemy: ParserTypes, instance, map_at, Repeat, Sequence, Either, Optional, alternate, FlatMap, Repeat_until
# import ..ParserAlchemy: result_type, regex_string

    
export ReferringToken
struct ReferringToken{Tt, Tv, I} <: AbstractToken
    name::Tt
    value::Tv
    reference::I
end
value(x::Union{Token, TokenPair, ReferringToken}) = x.value
variable(x::Union{Token, ReferringToken}) = x.name
variable(x::TokenPair) = x.key



export TokenString
const TokenString = Vector{<:AbstractToken}

function Base.string(x::TokenString)
    b = IOBuffer()
    for e in x
        print(b,e)
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
    [] #Iterators.flatten( tokens(x.second) for x in d )
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




import Base: convert
Base.convert(::Type{TokenString}, x::String) = tokenize(x)

import CombinedParsers: _iterate, MatchState, state_type

export IteratorParser
"""
    IteratorParser{T}(label::String,match::Function,f::Function)

An iterator parser `p` on an AbstractArray Sequence `str`.
filters for `p.match(str[i])`, and return transformed `p.f(str[i],i).

TODO: deprecate for CharIn
"""
struct IteratorParser{T} <: CombinedParser{MatchState,T}
    match::Function
    label::String
    IteratorParser{T}(f::Function, l::String) where {T} =
        new{T}(x->x isa T && f(x),l)
    IteratorParser(f::Function, T::Type, l::String) =
        new{T}(x->x isa T && f(x),l)
    IteratorParser(T::Type, l::String) =
        new{T}(x->x isa T,l)
    IteratorParser(T::Type) =
        new{T}(x->x isa T,"$T")
end
Base.show(io::IO, x::IteratorParser) = print(io, x.label)
## CombinedParsers.state_type(::Type{<:IteratorParser}) = MatchState
# IteratorParser{T}(label::String,match::Function,f::Function) =
#     filter

CombinedParsers._iterate(
    tok::IteratorParser,
    str, till,
    i,after,state::MatchState) = nothing
function CombinedParsers._iterate(tok::IteratorParser,
                  str, till,
                  i,after,state::Nothing) 
    ##@show typeof(str[i])
    if i<=lastindex(str) && tok.match(str[i]) 
        Base.nextind(str,i), MatchState()
    else
        nothing
    end
end

function Base.get(tok::IteratorParser, str, till, after,i,state::MatchState) 
    str[i]
end
@inline Base.nextind(str,i::Int,parser::IteratorParser,x) =
    Base.nextind(str,i)
@inline Base.prevind(str,i::Int,parser::IteratorParser,x) = 
    Base.prevind(str,i)

include("lines.jl")

include("html.jl")


import CombinedParserTools: word, footnote, quotes, capitalized, delimiter
import CombinedParsers.Regexp: word_char

export default_tokens
default_tokens(mask=AnyChar()) =
    @annotate [ :number     => !!Repeat1(re"[0-9]" .& mask), 
                :literal    => !!Repeat1(word_char .& mask), 
                :delimiter  => !!(delimiter .& mask),
                :footnote   => !!footnote .& mask, 
                :quote      => !!(quotes .& mask)
                ]

export tokenstring,default_token,quotes
tokenstring(unknown=AnyChar()) = Repeat(default_token(unknown) | @annotate [ :unknown => !!unknown])

end
