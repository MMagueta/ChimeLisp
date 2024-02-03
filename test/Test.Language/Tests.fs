module Tests

open Xunit

module Assert =
    let Equal (expected: int) (actual: int) = Assert.Equal(expected, actual)
    

[<Fact>]
let ``Primitives`` () =
    Language.Parser.generateAST "[+ 1.5 2.0]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

    Language.Parser.generateAST "\"Hello World! 🍬\""
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)


[<Fact>]
let ``Lambdas`` () =
    Language.Parser.generateAST "[lambda [x] [+ 1 1]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

    Language.Parser.generateAST "[[lambda [x] [+ x 1]] 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

    Language.Parser.generateAST "[println [int->string [[lambda [x] [+ x 1]] 1]]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

[<Fact>]
let ``Conditionals`` () =
    Language.Parser.generateAST "[if [= 10 10 11]
                     [println \"They are equal!\"]
                  [println \"They are not equal!\"]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

    Language.Parser.generateAST "[if [= 10 10 10]
                     [println \"They are equal!\"]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

[<Fact>]
let ``Definitions`` () =
    Language.Parser.generateAST "[defun hello [x]
                     [println [int->string x]]]
                 [hello 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)
    
    Language.Parser.generateAST "[defun hello [x y]
                        [progn
                          [println [int->string y]]
                          [println x]]]
                      [hello \"Hello\" 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

    Language.Parser.generateAST "[defvar hello \"Hello!\"]
                      [println hello]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> (fst >> Assert.Equal 0)

