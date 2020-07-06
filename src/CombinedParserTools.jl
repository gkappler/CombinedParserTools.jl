module CombinedParserTools
using CombinedParsers
using CombinedParsers.Regexp

import CombinedParsers.Regexp: word
opt(x...;kw...) = Optional(x...;kw...)
regex_neg_lookahead(e,match=AnyChar()) =
    !Repeat(Sequence(NegativeLookahead(e),match))

## export tokenizer_regex
##function tokenizer_regex()
##    re = (
lf          = re"\n"
whitespace  = re"[ \t]+"
whitespace_newline = re"[ \t]*\r?\n"
quotes      = re"[\"'`]"
# indentation = re"[ \t]*"
# content_characters = re"[^\t\r\n]+"
number      = re"[0-9]+"  ## TODO alt(...) csv
letters     = re"[A-Za-z*-]*"
# 
parenthesisP(open,close) = seq(String,
    open, re"[^][{}()]*", close;
    transform=(v,i) -> join(v))
delimiter   = re"[-, _/\.;:*\|!?&]"
#word        = re"\p{L}+" # re"[^!\[\]\(\){<>},*;:=\| \t_/\.\n\r\"'`⁰¹²³⁴⁵⁶⁷⁸⁹]+"
footnote    = re"[⁰¹²³⁴⁵⁶⁷⁸⁹]+"
enum_label = re"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"
wdelim = re"[ \t\r\n]+"

pad(x) = seq(opt(whitespace), x, opt(whitespace), transform = v->v[2])



export emptyline
emptyline = re"[ \t]*\r?\n"

extension   = re"\.[[:alnum:]~#]+"

email_regexp = re"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"

## is this official??
author_email = seq(:name => JoinSubstring(rep(CharNotIn('<'))),
                   " <", :email => rep_until(email_regexp, re">"))

include("tokens.jl")
end # module
