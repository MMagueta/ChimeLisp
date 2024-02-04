namespace Language

module REPL = begin
    open FSharp.Text.Lexing
    let read text =
        try
            text
            |> LexBuffer<char>.FromString
            |> Parser.parse Lexer.tokenStream
            |> Ok
        with e -> Error e.Message

    let delimitersBalanced (expr: string) =
        let rec balance counter stream =
            match stream with
            | [] -> counter = 0
            | ']'::_  when counter = 0 -> false
            | '['::rest -> balance (counter + 1) rest
            | ']'::rest -> balance (counter - 1) rest
            | _::_ -> false
        expr
        |> Seq.filter (fun c -> c = '[' || c = ']')
        |> Seq.toList
        |> balance 0
    
    let repl () =
        let rec loop waitingForNext text env =
            if not waitingForNext then
                printf "🔔."
            let line = System.Console.ReadLine()
            if line = ":exit" && delimitersBalanced (text + line) then
                ()
            else
                if not <| delimitersBalanced (text + line) then
                    loop true (text + line) env
                else
                    match read (text + line) with
                    | Ok ast ->
                        let _, env =
                            ast
                            |> List.map Language.Generator.expand
                            |> Language.Generator.wrapper true
                        loop false "" env
                    | Error e -> printfn "[ERROR]: %A" e
        loop false "" Map.empty

end
