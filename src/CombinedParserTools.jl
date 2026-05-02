"""
Experimental module providing several types to represent annotated text as 
[`@annotate`](@ref)d [`Token`](ref)s and nested [`Node`](@ref)s in [`Line`](ref)s.
"""
module CombinedParserTools
using AutoHashEquals
using CombinedParsers
using CombinedParsers.Regexp
import InternedStrings: intern

import CombinedParsers: word_char
import CombinedParsers: whitespace_maybe, whitespace_horizontal, word

# extension   = re"\.[[:alnum:]~#]+"
# emptyline          = !Sequence(horizontal_space_maybe, newline)
# whitespace_newline = !Sequence(horizontal_space_maybe, newline)
# 

# capitalized_word = !Sequence(CharIn(isuppercase), Repeat1(CharIn(islowercase)))

# ## is this official??
# email_regexp = re"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"
# author_email = Sequence(
#     :name => !Repeat(CharNotIn('<')),
#     re" *<", :email => Repeat_until(email_regexp, ">"))

## export tokenizer_regex
##function tokenizer_regex()
##    re = (
# indentation = re"[ \t]*"
# content_characters = re"[^\t\r\n]+"
# number      = re"[0-9]+"  ## TODO alt(...) csv
# letters     = re"[A-Za-z*-]*"

include("tokens.jl")
include("parentheses.jl")
include("lines.jl")
include("html.jl")



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
        esc(:(Either( ( Token(parser(p)) for p in $top)...)))
    else
        dump(x)
        :()
    end
end

export default_tokens
default_tokens(mask=AnyChar()) =
    @annotate [ :number     => !!Repeat1(re"[0-9]" .& mask), 
                :literal    => !!Repeat1(word_char .& mask), 
                :delimiter  => !!(re"[-, _/\.;:*\|!?&]" .& mask),
                :footnote   => !!Repeat1(re"[¹²³⁴⁵⁶⁷⁸⁹⁰⁰¹²³⁴⁵⁶⁷⁸⁹]" .& mask), 
                :quote      => !!(re"[\"'`]" .& mask)
                ]

# export ReferringToken
# struct ReferringToken{Tt, Tv, I} <: AbstractToken
#     name::Tt
#     value::Tv
#     reference::I
# end
# value(x::Union{Token, TokenPair, ReferringToken}) = x.value
# variable(x::Union{Token, ReferringToken}) = x.name
# variable(x::TokenPair) = x.key

# include("deprecated.jl")

# export TimingParser
# @auto_hash_equals struct TimingParser{P,S,R,T} <: CombinedParsers.WrappedParser{P,S,R}
#     parser::P
#     aggregator::T
#     TimingParser(p::CombinedParser,t) =
#         new{typeof(p), CombinedParsers.state_type(p), result_type(p), typeof(t)}(p,t)
# end
# using StatsBase
# import StatsBase: fit!
# import CombinedParsers: iterate_state
# @inline function CombinedParsers.iterate_state(parser::TimingParser, sequence, till, posi, next_i, state)
#     r = @timed CombinedParsers.iterate_state(parser.parser, sequence, till, posi, next_i, state)
#     1
#     StatsBase.fit!(parser, r.time)
#     r.value
# end
# import CombinedParsers: constructor_name, _deepmap_parser
# CombinedParsers.constructor_name(x::TimingParser) = "TimingParser $(x.aggregator.stats)"
# StatsBase.fit!(x::TimingParser, value) = StatsBase.fit!(x.aggregator, value)

# CombinedParsers._deepmap_parser(f,mem::AbstractDict,x::TimingParser,a...;kw...) =
#     TimingParser(deepmap_parser(f,mem,x.parser,a...;kw...), x.aggregator)

# timed(x::CombinedParser, aggregator) = deepmap_parser(_timed, x, aggregator)
# _timed(x::TimingParser, aggregator) = TimingParser(x.parser, aggregator())
# _timed(x::CombinedParser, aggregator) = TimingParser(x, aggregator())


end # module
