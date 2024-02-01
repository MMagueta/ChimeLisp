module Tests

open Xunit


[<Fact>]
let ``My test`` () =
    Language.Parser.generateAST "[+ 1.5 2.0]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore
            
    Language.Parser.generateAST "[lambda [x] [+ 1 1]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "[[lambda [x] [+ x 1]] 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "[println [int->string [[lambda [x] [+ x 1]] 1]]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "[if [= 10 11]
                     [println \"They are equal!\"]
                  [println \"They are not equal!\"]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "[if [= 10 10]
                     [println \"They are equal!\"]]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "[defun hello [x]
                     [println [int->string x]]]
                 [hello 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore
    
    Language.Parser.generateAST "[defun hello [x y]
                        [progn
                          [println [int->string y]]
                          [println x]]]
                      [hello \"Hello\" 1]"
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore

    Language.Parser.generateAST "\"Hello World! 🍬\""
    |> List.map Language.Generator.expand
    |> Language.Generator.wrapper
    |> ignore
