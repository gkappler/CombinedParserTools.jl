cd("/home/gregor/dev/julia")
using Pkg
Pkg.activate("CombinedParserTools")

using CombinedParsers
using CombinedParsers.Regexp
using CombinedParserTools
using CombinedParserTools.Tokens
using Test


using CombinedParsers
using CombinedParsers.Regexp
import InternedStrings: intern

import CombinedParsers: word_char
import CombinedParsers: whitespace_maybe, whitespace_horizontal, word

extension   = re"\.[[:alnum:]~#]+"
footnote    = re"[¹²³⁴⁵⁶⁷⁸⁹⁰⁰¹²³⁴⁵⁶⁷⁸⁹]"
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


import CombinedParserTools.Tokens: path_file
@testset "CombinedParserTools.jl" begin
    @test trim(url())("   https://en.wikipedia.org/wiki/Help:Wikitext#Link_to_another_namespace ") ==
        (scheme    = "https",
         authority = (userinfo = "", host = "en.wikipedia.org", port = 80),
         path      = "wiki/Help:Wikitext",
         query     = "",
         fragment  = "Link_to_another_namespace"
         )
end

@testset "CombinedParserTools.jl" begin
    import CombinedParserTools.Tokens: attributes
    @testset "html attributes" begin
        @test parse(attributes,"a = 1 b=6% font=\"+1\"") == [T"1"a, T"6%"b, T"+1"font]
        @test parse(attributes, "size=10% class=1") == [ T"10%"size, T"1"class ]
    end

    @testset "html" begin
        inner = Either{Any}(Any[!Repeat(CharNotIn("<>"))]);
        pushfirst!(inner,html(!re"[[:alpha:]]+",inner,attributes));
        parse(inner,"<a font=1><b>b</b>a</a>")
        @test parse(inner,"<a font=\"+1\">i<b>bold</b>j</a>") == 
            Node("a",[T"+1"font], ["i",Node("b", [], ["bold"]),"j"])

        @test parse(inner,"<a font=+1/>") ==
            Node("a", [T"+1"font], [])
    end                  
    # Write your own tests here.
end

import CombinedParserTools: word, footnote, quotes, capitalized, delimiter

@syntax .!(parser.([ :number      => !!re"[0-9]+", 
                      capitalized, 
                      :literal     => !!word, 
                      delimiter,
                      :footnote    => !!footnote, 
                      :quote       => !!quotes,
                      ])) in default_tokens

import CombinedParserTools: capitalized, delimiter
@annotate [ :number      => !!re"[0-9]+", 
            capitalized_word, 
            :literal     => !!word, 
            delimiter,
            :footnote    => !!footnote, 
            :quote       => !!quotes,
            ]

@syntax !.[ :number      => !!re"[0-9]+", 
            capitalized, 
            :literal     => !!word, 
            :footnote    => !!footnote, 
            :quote       => !!quotes,
            delimiter, ## re"[\|\n]"
            :parentheses => !!re"[][{}()<>]", 
            :operator    => !!re"[-+*/%&!=]", 
            :unknown     => !!re"[^][(){}\n \t\|]"
            ] in simple_tokens 

#map(x->!parser(x),[
## :number             => Sequence(" "*enum_label*" "),

using CombinedParserTools.Tokens
simple_tokens          = Either(Token)
@syntax !.[ :number      => !!re"[0-9]+", 
            capitalized, 
            :literal     => !!word, 
            :footnote    => !!footnote, 
            :quote       => !!quotes,
            delimiter, ## re"[\|\n]"
            :parentheses => !!re"[][{}()<>]", 
            :operator    => !!re"[-+*/%&!=]", 
            :unknown     => !!re"[^][(){}\n \t\|]"
            ] in simple_tokens 

using OnlineStats

re_timer = CombinedParserTools.timed(CombinedParsers.Regexp.re_parser, () -> Series(Sum(),Mean()))

