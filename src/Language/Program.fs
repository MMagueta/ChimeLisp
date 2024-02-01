namespace PerplexDB.Language

module Main =
    open System.IO
    open FSharp.Text.Lexing

    open System.Reflection
    open System.Reflection.Emit

    let generateAST text =
        let lexbuf = LexBuffer<char>.FromString text
        let ast = Parser.parse Lexer.tokenStream lexbuf
        ast

    [<EntryPoint>]
    let main _ =
        
        // let ast = generateAST "[lambda [x] [+ x 1]] [lambda [y] [+ y 1]]"
        
        // generateAST ""
        // |> Language.Expression.expand Map.empty
        // |> Language.Expression.compile Map.empty
        // |> _.GetMethod("Main").Invoke((), [|0; ([||]: string array)|])
        // |> ignore
        
        // let expanded =
            // generateAST "[+ 1 2]"
            // |> Language.Expression.expand Map.empty

        // printfn "%A" expanded
            
        // expanded |> Language.Expression.compile Map.empty
        // |> _.GetMethod("Main").Invoke((), [|0; ([||]: string array)|])
        // |> ignore
        
        // generateIL()
        // |> _.GetMethod("Main").Invoke((), [|0; ([||]: string array)|])
        // |> printfn "%A"
        (*
        generateAST "[println \"Hello World! :)\"]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> printfn "%A"

        generateAST "[println [+ 1.5 2.0 1.0]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> printfn "%A"
        
        generateAST "[println [/ 2 [+ 1 2]]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> printfn "%A"

        generateAST "[if [= 10 10]
                         [println \"They are equal!\"]
                      [println \"They are not equal!\"]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> printfn "%A"
        
        generateAST "[defun abc [x] [+ x 1]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> printfn "%A"
        *)
        
        let generateIL () =
            let assembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Chimelisp.exe"), AssemblyBuilderAccess.Run)
            let mo = assembly.DefineDynamicModule("Chimelisp.exe")
            let typeBuilder = mo.DefineType("Chimelisp", TypeAttributes.Sealed ||| TypeAttributes.Public)

            let secondary =
                typeBuilder.DefineMethod("Secondary", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>|])
            let ilSecondary = secondary.GetILGenerator()
            ilSecondary.Emit(OpCodes.Ldarg_0)
            ilSecondary.Emit(OpCodes.Ldc_I4_1)
            ilSecondary.Emit(OpCodes.Add)
            ilSecondary.Emit(OpCodes.Ret)

            let entry =
                typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>; typeof<string array>|])
            let il = entry.GetILGenerator()
            il.Emit(OpCodes.Ldc_I4, 10)
            //il.Emit(OpCodes.Call, secondary)
            il.Emit(OpCodes.Ret)

            let concreteEntry = typeBuilder.CreateType()
            concreteEntry
            
        let generateIL () =
            let assembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Chimelisp.exe"), AssemblyBuilderAccess.Run)
            let mo = assembly.DefineDynamicModule("Chimelisp.exe")
            let typeBuilder = mo.DefineType("Chimelisp", TypeAttributes.Sealed ||| TypeAttributes.Public)
            let entry =
                typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>; typeof<string array>|])

            let secondary =
                typeBuilder.DefineMethod("Secondary", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>|])
            let ilSecondary = secondary.GetILGenerator()
            ilSecondary.Emit(OpCodes.Ldarg, 0)
            ilSecondary.Emit(OpCodes.Ldc_I4_S, 1)
            ilSecondary.Emit(OpCodes.Add)
            ilSecondary.Emit(OpCodes.Ret)

            let il = entry.GetILGenerator()
            il.Emit(OpCodes.Ldc_I4, 1)
            il.Emit(OpCodes.Call, secondary)
            
            il.Emit(OpCodes.Ret)

            let concreteEntry = typeBuilder.CreateType()
            concreteEntry
        
        //generateIL()
        //|> _.GetMethod("Main").Invoke((), [|0; ([||]: string array)|])
        //|> printfn "%A"

        generateAST "[+ 1.5 2.0]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore
        
        generateAST "[lambda [x] [+ 1 1]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore

        generateAST "[[lambda [x] [+ x 1]] 1]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore

        generateAST "[println [int->string [[lambda [x] [+ x 1]] 1]]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore

        generateAST "[if [= 10 11]
                         [println \"They are equal!\"]
                      [println \"They are not equal!\"]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore

        generateAST "[if [= 10 10]
                         [println \"They are equal!\"]]"
        |> List.map Language.Generator.expand
        |> Language.Generator.wrapper
        |> ignore

        0
