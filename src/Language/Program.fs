namespace Language

module Parser = begin
    open FSharp.Text.Lexing
    
    let generateAST text =
        let lexbuf = LexBuffer<char>.FromString text
        let ast = Parser.parse Lexer.tokenStream lexbuf
        ast 
end

module Main =
    open System.Reflection
    open System.Reflection.Emit

    [<EntryPoint>]
    let main _ =
        (*
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

        *)
            
        let generateIL () =
            let assembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("Chimelisp.exe"), AssemblyBuilderAccess.Run)
            let mo = assembly.DefineDynamicModule("Chimelisp.exe")
            let typeBuilder = mo.DefineType("Chimelisp", TypeAttributes.Sealed ||| TypeAttributes.Public)
            let entry =
                typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>; typeof<string array>|])

            let secondary =
                typeBuilder.DefineMethod("Secondary", MethodAttributes.Static ||| MethodAttributes.Public, typeof<float32>, [|typeof<obj>|])
            let ilSecondary = secondary.GetILGenerator()
            ilSecondary.Emit(OpCodes.Ldarg, 0)
            ilSecondary.Emit(OpCodes.Unbox_Any, typeof<int>)
            ilSecondary.Emit(OpCodes.Ldc_R4, 0)
            ilSecondary.Emit(OpCodes.Add)
            ilSecondary.Emit(OpCodes.Ret)

            let il = entry.GetILGenerator()
            il.Emit(OpCodes.Ldc_I4, 2)
            il.Emit(OpCodes.Box, typeof<int>)
            il.Emit(OpCodes.Call, secondary)
            il.Emit(OpCodes.Call, typeof<System.Console>.GetMethod("WriteLine", [| typeof<float32> |]))


            il.Emit(OpCodes.Ldc_I4, 0)
            il.Emit(OpCodes.Ret)

            let concreteEntry = typeBuilder.CreateType()
            concreteEntry
        
        generateIL()
        |> _.GetMethod("Main").Invoke((), [|0; ([||]: string array)|])
        |> printfn "%A"


        // REPL.repl()

        0
