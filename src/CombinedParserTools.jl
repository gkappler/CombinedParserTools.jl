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

extension   = re"\.[[:alnum:]~#]+"
footnote    = re"[¹²³⁴⁵⁶⁷⁸⁹⁰⁰¹²³⁴⁵⁶⁷⁸⁹]+"
quotes      = re"[\"'`]"
enum_label = !re"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"
emptyline          = !Sequence(horizontal_space_maybe, newline)
whitespace_newline = !Sequence(horizontal_space_maybe, newline)
delimiter = re"[-, _/\.;:*\|!?&]"

capitalized_word = !Sequence(CharIn(isuppercase), Repeat1(CharIn(islowercase)))

## is this official??
email_regexp = re"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"
author_email = Sequence(
    :name => !Repeat(CharNotIn('<')),
    re" *<", :email => Repeat_until(email_regexp, ">"))

## export tokenizer_regex
##function tokenizer_regex()
##    re = (
# indentation = re"[ \t]*"
# content_characters = re"[^\t\r\n]+"

include("tokens.jl")

default_tokens(mask=AnyChar()) =
    @annotate [ :number     => !!Repeat1(re"[0-9]" .& mask), 
                :literal    => !!Repeat1(word_char .& mask), 
                :delimiter  => !!(delimiter .& mask),
                :footnote   => !!footnote .& mask, 
                :quote      => !!(quotes .& mask)
                ]

include("parentheses.jl")
include("lines.jl")

include("html.jl")

export ReferringToken
struct ReferringToken{Tt, Tv, I} <: AbstractToken
    name::Tt
    value::Tv
    reference::I
end
value(x::Union{Token, TokenPair, ReferringToken}) = x.value
variable(x::Union{Token, ReferringToken}) = x.name
variable(x::TokenPair) = x.key

include("deprecated.jl")

end # module
