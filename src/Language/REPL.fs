namespace Language

module REPL = begin
    open FSharp.Text.Lexing
    
    let generateAST text =
        let lexbuf = LexBuffer<char>.FromString text
        let ast = Parser.parse Lexer.tokenStream lexbuf
        ast

    let repl () =
        let rec loop text env =
            printf "🔔."
            let line = System.Console.ReadLine()
            if line = "exit" then
                ()
            else
                if not <| System.String.IsNullOrWhiteSpace(line) then
                    loop (text + line) env
                else
                    let result, env =
                        generateAST text
                        |> List.map Language.Generator.expand
                        |> Language.Generator.wrapper
                    // printfn "%A" result
                    loop text env
        loop "" Map.empty

end
