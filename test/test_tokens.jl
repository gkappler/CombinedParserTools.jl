
using CombinedParserTools.Tokens
import Base: parse
parse(x::AbstractString) = parse(tokenstring,x)
@test parse(tokenstring, "") == Token[]
@test parse(tokenstring, "a") == Token[l"a"]

# todo:
# @test convert(NamedTuple{(:a,),Tuple{Int}},(a=2,b=1)) == (a=2,)

@test parse("") == Token[]
@test parse("test eines") ==  Token[l"test", delim" ",l"eines"]
@test parse(tokenstring,"test eines") ==  Token[l"test", delim" ",l"eines"]

@test parse("a") == [ Token(:literal, "a") ]
@test parse("a b") == [ l"a",  delim" ", l"b" ]
@test parse(".a b") == [ delim".", l"a",  delim" ", l"b" ]
@test parse(".aü öb") == [ delim".", l"aü",  delim" ", l"öb" ]


@test parse("/a") == [ delim"/", l"a" ]
@test parse("/a/b/") == [ delim"/", l"a", delim"/", l"b", delim"/" ]


## TODO

## ?? @test parse("<a=b>") == [ Token(:a, "b") ]
## ?? @test parse("<b>") == [ Token(:b) ]
## ?? @test parse("<b=test>") == [ Token(:b,"test") ]

## ?? @test parse("<b=test>.pdf", V) == [ Token(:b,"test"), (@T ext ".pdf") ]

## ?? @test parse("/a/<b>") == [ delim"/", l"a", delim"/", Token(:b) ]

if false
    (parse("years like 2012 are parsed"))

    @test parse(l"a.b") == [ l"a", Token(:ext,".b") ]
    @test parse("years are parsed like 2012")[end] == Token(:number,"2012")


    @test T"2012"yyyy == Token(:yyyy,"2012")

    @test parse("invoice 2012.pdf") == [ l"invoice", delim" ",  T"2012"number, T".pdf"ext ]
    @test parse("invoice 2012.pdf.pdf") ==
        [ l"invoice", delim" ",  (@T yyyy 2012), delim".", l"pdf", (@T ext ".pdf") ]


    @test parse("1.2.3.7z",V) == [ (@T version "1.2.3"), @T ext ".7z" ]
    @test parse("draft 99.pdf",V) ==
        [ l"draft", delim" ", (@T version "99"), @T ext ".pdf" ]
end
# adding pattern to Filing applies new pattern to Files
# adding File to Filing applies old pattern to File

# query Files matching a pattern

# query Files matching a pattern

import CombinedParserTools: bracket_number, bracket_reference

@test (parse(bracket_number,"[1]")) ==
    Token(:number, "[1]")

@test (parse(bracket_reference,"[1, 2]")) ==
    Token(:reference, "[1, 2]")


