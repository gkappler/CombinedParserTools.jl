module CombinedParserTools
using CombinedParsers
using CombinedParsers.Regexp

import CombinedParsers.Regexp: word

footnote    = re"[⁰¹²³⁴⁵⁶⁷⁸⁹]+"
quotes      = re"[\"'`]"
enum_label = re"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"
emptyline          = re"[ \t]*\r?\n"
whitespace_newline = re"[ \t]*\r?\n"
email_regexp = re"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"
delimiter = re"[-, _/\.;:*\|!?&]"
capitalized = Sequence(CharIn(isuppercase), Repeat1(CharIn(islowercase)))

## is this official??
author_email = Sequence(
    :name => !Repeat(CharNotIn('<')),
    " <", :email => Repeat_until(email_regexp, ">"))

## export tokenizer_regex
##function tokenizer_regex()
##    re = (
newline     = re"\r?\n"
inline     = re"[^\r\n]"
lf          = re"\n"
whitespace  = re"[ \t]+"
# indentation = re"[ \t]*"
# content_characters = re"[^\t\r\n]+"
number      = re"[0-9]+"  ## TODO alt(...) csv
letters     = re"[A-Za-z*-]*"


parenthesisP(open,close) =
    Sequence(open, Repeat_until(AnyChar(),close,wrap=JoinSubstring))
pad(x) = Sequence(2,whitespace_maybe, x, whitespace_maybe)

extension   = re"\.[[:alnum:]~#]+"

include("tokens.jl")

############################################################
## legacy constructors

@deprecate opt(x...;kw...) Optional(x...;kw...)
@deprecate alt(a...) Either(a...)
# @deprecate seq(a...; kw...) Sequence(a...; kw...)
export seq
function seq(tokens::Vararg;
             transform=nothing, kw...)
    if transform isa Integer
        Sequence(transform,tokens...)
    elseif transform===nothing
        Sequence(tokens...)
    elseif transform isa Function
        map(transform, Sequence(tokens...))
    end
end

include("deprecated.jl")

include("greedy.jl")



import CombinedParsers: Numeric
export url
"""
Parse generic URL syntax, see [Wikipedia](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier#Generic_syntax).
"""
url(;
    blacklist=re"[^\v\h]",
    scheme = !!re"[a-z]+",
    match_parse = !!Repeat(AnyChar()),
    host_parse = join(match_parse,'.'),
    path_parse = match_parse, #path_file(),
    query_parse = match_parse
    # ["http","https","ftp","ftps",
    #  "mailto","irc",
    #  "file","data"]
    ) =
        Sequence(:scheme => Atomic(scheme),
                 ':', Atomic(Either(
                     "//",
                     PositiveLookbehind(caseless("mailto:")))),
                 :authority => Sequence(
                     :userinfo => Optional(Sequence(1,!!re"\w+", '@')),
                     :host => map(host_parse, !(Repeat1(blacklist .& CharNotIn("/:")))),
                     ## TODO: parse IP here?
                     :port => (':',Numeric(Int64))[2]  | 80
                 ),
                 :path => Optional(Sequence(
                     2, '/', map(path_parse, !Repeat(blacklist .& CharNotIn("?#"))))),
                 :query => Optional(
                     Sequence(2,'?',   !!(Repeat(blacklist .& CharNotIn("#"))))), #   ∘ query_parse )),
                 :fragment => Optional(Sequence(2, '#', !!Repeat(blacklist))),
                 NegativeLookahead(CharIn(",!?.)]:")) ## not the best solution
                 )

_prefix(p,x...) = ( isempty(x) || x == tuple("") ) ? tuple() : (p,x...)
_suffix(x,p) = x == "" ? "" : x*p
Base.show(io::IO, x::NamedTuple{fieldnames(result_type(url()))}) =
    print(io, x.scheme,"://",
          _suffix(x.authority.userinfo,"@")...,
          x.authority.host...,
          x.authority.port == 80 ? "" : x.authority.port,
          "/", x.path...,
          _prefix("?", x.query, _prefix("#", x.fragment...)...)...
          )

end # module
