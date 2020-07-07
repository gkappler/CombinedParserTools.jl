using CombinedParsers
using CombinedParsers.Regexp
using CombinedParserTools
using CombinedParserTools.Tokens
using Test

@testset "CombinedParserTools.jl" begin
    import CombinedParserTools.Tokens: attributes
    @testset "html attributes" begin
        @test parse(attributes,"a = 1 b=6% font=\"+1asd\"") == [T"1"a, T"6%"b, T"+1asd"font]
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
