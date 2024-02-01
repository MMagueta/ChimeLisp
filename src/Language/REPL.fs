namespace Language

module REPL = begin
    open FSharp.Text.Lexing
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
                        text
                        |> LexBuffer<char>.FromString
                        |> Parser.parse Lexer.tokenStream
                        |> List.map Language.Generator.expand
                        |> Language.Generator.wrapper
                    // printfn "%A" result
                    loop text env
        loop "" Map.empty

end
