namespace Language

open System.IO
open System
open Mono.Cecil
open Mono.Cecil.Rocks
open Mono.Cecil.Cil

[<RequireQualifiedAccess>]
module Expression = begin
    type Storage = 
    | Argument of string
    | Local of string
    | Global of string
    type t =
    | EAtom of string
    | EArgument of Index: int * Type
    | EList of t list
    | EInteger of int
    | EFloat of float32
    | EString of string
    | EVariable of string * t
    | EApplication of Closure: t * Arguments: t list
    | EAbstraction of string list * t
    | ENative of string * t list
    | EIfThenElse of t * t * t option
    | EProgn of t list
    | EDefinition of Name: string * StorageType: Storage * Content: t option
    | ELoad of Storage
end

module Generator = begin

    // Type to Hold Context While Emitting IL
    type EmitCtx =
        { Assm: AssemblyDefinition;
          IL: ILProcessor;
          mutable NextLambda: int;
          ScopePrefix: string;
          ProgramTy: TypeDefinition }

    /// Emit an instance of the unspecified value
    let private emitUnspecified (il: ILProcessor) =
        // TODO: What should we do about empty sequences? This falls back to '()
        il.Emit(OpCodes.Ldnull)

    /// Ensure a field exists on the program type to be used as a global variable
    let private ensureField ctx id =
        let pred (field: FieldDefinition) =
            field.Name = id
        match Seq.tryFind pred ctx.ProgramTy.Fields with
        | Some(found) -> found
        | None ->
            let newField = FieldDefinition(id, FieldAttributes.Static, ctx.Assm.MainModule.TypeSystem.Object)
            ctx.ProgramTy.Fields.Add(newField)
            newField

    /// Emit a Single Bound Expression
    ///
    /// Emits the code for a single function into the given assembly.
    let rec private emitExpression (ctx: EmitCtx) (expr: Expression.t) =
        let recurse = emitExpression ctx
        match expr with
        | Expression.EAtom "nil" -> ctx.IL.Emit(OpCodes.Ldnull)
        | Expression.EInteger n ->
            ctx.IL.Emit(OpCodes.Ldc_I4, n)
            ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Int32)
        | Expression.EString s -> ctx.IL.Emit(OpCodes.Ldstr, s)
        //| Expression.EBoolean b ->
        //    ctx.IL.Emit(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
        //    ctx.IL.Emit(OpCodes.Box, ctx.Assm.MainModule.TypeSystem.Boolean)
        | Expression.EList [] -> emitUnspecified ctx.IL
        | Expression.EList s -> emitSequence ctx s
        | Expression.EApplication(ap, args) -> emitApplication ctx ap args
        | Expression.EDefinition(id, storage, maybeVal) ->
            // TODO: could we just elide the whole definition if there is no value.
            //       do we need to start considering expressions and statements
            //       as disjoint things?
            match maybeVal with
            | Some(expr) -> recurse expr
            | None -> ctx.IL.Emit(OpCodes.Ldnull)
            ctx.IL.Emit(OpCodes.Dup)
            match storage with
            | Expression.Storage.Global id ->
                let field = ensureField ctx id
                ctx.IL.Emit(OpCodes.Stsfld, field)
            | Expression.Storage.Local(idx) ->
                ctx.IL.Emit(OpCodes.Stloc, idx)
            | Expression.Storage.Argument(idx) ->
                ctx.IL.Emit(OpCodes.Starg, idx)
        | Expression.ELoad storage ->
            match storage with
            | Expression.Storage.Global id ->
                let field = ensureField ctx id
                ctx.IL.Emit(OpCodes.Ldsfld, field)
            | Expression.Storage.Local(idx) ->
                ctx.IL.Emit(OpCodes.Ldloc, idx)
            | Expression.Storage.Argument(idx) ->
                ctx.IL.Emit(OpCodes.Ldarg, idx)
        //| Expression.If(cond, ifTrue, maybeIfFalse) ->
        //    recurse cond
        //    let lblFalse = ctx.IL.Create(OpCodes.Nop)
        //    let lblEnd = ctx.IL.Create(OpCodes.Nop)
        //    ctx.IL.Emit(OpCodes.Brfalse_S, lblFalse)
        //    recurse ifTrue
        //    ctx.IL.Emit(OpCodes.Br_S, lblEnd)
        //    ctx.IL.Append(lblFalse)
        //    match maybeIfFalse with
        //    | Some ifFalse -> recurse ifFalse
        //    | None -> ctx.IL.Emit(OpCodes.Ldnull)
        //    ctx.IL.Append(lblEnd)
        | Expression.EAbstraction(formals, body) ->
            emitLambda ctx formals body
    and emitSequence ctx seq =
        let popAndEmit x =
            ctx.IL.Emit(OpCodes.Pop)
            emitExpression ctx x
        emitExpression ctx (List.head seq)
        List.tail seq
        |>  Seq.iter popAndEmit
    and emitAuxLambda (ctx: EmitCtx) name formals body =
        let methodDecl = MethodDefinition(name,
                                          MethodAttributes.Public ||| MethodAttributes.Static,
                                          ctx.Assm.MainModule.TypeSystem.Object)
        ctx.ProgramTy.Methods.Add methodDecl

        let addParam id =
            methodDecl.Parameters.Add(ParameterDefinition(id, ParameterAttributes.None, ctx.Assm.MainModule.TypeSystem.Object))

        // Add formals as parameter definitions
        List.map addParam formals |> ignore

        let ctx = { IL = methodDecl.Body.GetILProcessor()
                  ; ProgramTy = ctx.ProgramTy
                  ; NextLambda = 0
                  ; ScopePrefix = name
                  ; Assm = ctx.Assm }
        emitExpression ctx body
        ctx.IL.Emit(OpCodes.Ret)
        methodDecl.Body.Optimize()

        // Emit a thunk that unpacks the arguments to our method
        // This allows us to provide a uniform calling convention for
        // lambda instances
        let thunkDecl = MethodDefinition((sprintf "%s:thunk" name),
                                          MethodAttributes.Public ||| MethodAttributes.Static,
                                          ctx.Assm.MainModule.TypeSystem.Object)
        thunkDecl.Parameters.Add(ParameterDefinition(ArrayType(ctx.Assm.MainModule.TypeSystem.Object)))
        ctx.ProgramTy.Methods.Add thunkDecl
        let thunkIl = thunkDecl.Body.GetILProcessor()

        let unpackArg (idx: int) id =
            thunkIl.Emit(OpCodes.Ldarg_0)
            thunkIl.Emit(OpCodes.Ldc_I4, idx)
            thunkIl.Emit(OpCodes.Ldelem_Ref)
            idx + 1

        List.fold unpackArg 0 formals |> ignore
        thunkIl.Emit(OpCodes.Call, methodDecl)
        thunkIl.Emit(OpCodes.Ret)
        thunkDecl.Body.Optimize()

        methodDecl, thunkDecl
    and emitApplication ctx ap args =
        match ap with
        | Expression.EAtom value -> 
            // Emit the arguments array
            ctx.IL.Emit(OpCodes.Ldc_I4, List.length args)
            ctx.IL.Emit(OpCodes.Newarr, ctx.Assm.MainModule.TypeSystem.Object)
            List.fold (fun (idx: int) e -> 
                ctx.IL.Emit(OpCodes.Dup)
                ctx.IL.Emit(OpCodes.Ldc_I4, idx)
                emitExpression ctx e
                ctx.IL.Emit(OpCodes.Stelem_Ref)
                idx + 1) 0 args |> ignore
            
            let path = value.Split('.')
            let module' = 
                String.Join('.', (Array.rev >> Array.tail) path)
                |> ctx.Assm.MainModule.GetType
                |> _.GetType()
            let methodName = Array.last path
            let params = List.map (fun x -> ) args
            let x = ctx.Assm.MainModule.ImportReference(module'.GetMethod(methodName, ))

            let lambdaId = ctx.NextLambda
            ctx.NextLambda <- lambdaId + 1
            let _, thunk = emitAuxLambda ctx (sprintf "%s:lambda%d" ctx.ScopePrefix lambdaId) args 
            let paramTypes = [|typeof<obj>; typeof<IntPtr>|]
            let funcObjCtor = ctx.Assm.MainModule.ImportReference(typeof<System.Func<obj[], obj>>.GetConstructor(paramTypes))
            ctx.IL.Emit(OpCodes.Ldnull)
            ctx.IL.Emit(OpCodes.Ldftn, thunk :> MethodReference)
            ctx.IL.Emit(OpCodes.Newobj, funcObjCtor)

        | _ ->
            failwith "Bad application!"

        //let funcInvoke = ctx.Assm.MainModule.ImportReference(typeof<System.Func<obj[], obj>>.GetMethod("Invoke", [| typeof<obj[]> |]))
        //ctx.IL.Emit(OpCodes.Callvirt, funcInvoke)
    and emitLambda ctx formals body =
        // Emit a declaration for the lambda's implementation
        let lambdaId = ctx.NextLambda
        ctx.NextLambda <- lambdaId + 1
        let _, thunk = emitNamedLambda ctx (sprintf "%s:lambda%d" ctx.ScopePrefix lambdaId) formals body
        let paramTypes = [|typeof<obj>; typeof<IntPtr>|]
        let funcObjCtor = ctx.Assm.MainModule.ImportReference(typeof<System.Func<obj[], obj>>.GetConstructor(paramTypes))
        ctx.IL.Emit(OpCodes.Ldnull)
        ctx.IL.Emit(OpCodes.Ldftn, thunk :> MethodReference)
        ctx.IL.Emit(OpCodes.Newobj, funcObjCtor)
    and emitNamedLambda (ctx: EmitCtx) name formals body =
        let methodDecl = MethodDefinition(name,
                                          MethodAttributes.Public ||| MethodAttributes.Static,
                                          ctx.Assm.MainModule.TypeSystem.Object)
        ctx.ProgramTy.Methods.Add methodDecl

        let addParam id =
            methodDecl.Parameters.Add(ParameterDefinition(id, ParameterAttributes.None, ctx.Assm.MainModule.TypeSystem.Object))

        // Add formals as parameter definitions
        List.map addParam formals |> ignore

        let ctx = { IL = methodDecl.Body.GetILProcessor()
                  ; ProgramTy = ctx.ProgramTy
                  ; NextLambda = 0
                  ; ScopePrefix = name
                  ; Assm = ctx.Assm }
        emitExpression ctx body
        ctx.IL.Emit(OpCodes.Ret)
        methodDecl.Body.Optimize()

        // Emit a thunk that unpacks the arguments to our method
        // This allows us to provide a uniform calling convention for
        // lambda instances
        let thunkDecl = MethodDefinition((sprintf "%s:thunk" name),
                                          MethodAttributes.Public ||| MethodAttributes.Static,
                                          ctx.Assm.MainModule.TypeSystem.Object)
        thunkDecl.Parameters.Add(ParameterDefinition(ArrayType(ctx.Assm.MainModule.TypeSystem.Object)))
        ctx.ProgramTy.Methods.Add thunkDecl
        let thunkIl = thunkDecl.Body.GetILProcessor()

        let unpackArg (idx: int) id =
            thunkIl.Emit(OpCodes.Ldarg_0)
            thunkIl.Emit(OpCodes.Ldc_I4, idx)
            thunkIl.Emit(OpCodes.Ldelem_Ref)
            idx + 1

        List.fold unpackArg 0 formals |> ignore
        thunkIl.Emit(OpCodes.Call, methodDecl)
        thunkIl.Emit(OpCodes.Ret)
        thunkDecl.Body.Optimize()

        methodDecl, thunkDecl

    /// Emit the `Main` Method Epilogue
    ///
    /// This sequence of instructions is added at the end of the main method to
    /// coerce the result type into a return value for the application. Once we have
    /// some form of runtime library linked into the final executable it might be
    /// best to include this in there rather than emitting it manually each time.
    let private emitMainEpilogue (assm: AssemblyDefinition) (il: ILProcessor) =
        il.Emit(OpCodes.Dup)
        
        il.Emit(OpCodes.Call, assm.MainModule.ImportReference (typeof<Console>.GetMethod("WriteLine", [| typeof<obj> |])))
        
        //il.Emit(OpCodes.Dup)
        //il.Emit(OpCodes.Isinst, assm.MainModule.TypeSystem.Double)
        //let notInt = il.Create(OpCodes.Dup)
        //il.Emit(OpCodes.Brfalse, notInt)

        //il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Double)
        //il.Emit(OpCodes.Conv_I4)
        //il.Emit(OpCodes.Ret)

        //il.Append(notInt)
        //let notBool = il.Create(OpCodes.Pop)
        //il.Emit(OpCodes.Isinst, assm.MainModule.TypeSystem.Boolean)
        //il.Emit(OpCodes.Brfalse, notBool)
        //il.Emit(OpCodes.Unbox_Any, assm.MainModule.TypeSystem.Boolean)
        //let load0 = il.Create(OpCodes.Ldc_I4_0)
        //il.Emit(OpCodes.Brtrue, load0)
        //il.Emit(OpCodes.Ldc_I4_M1)
        //il.Emit(OpCodes.Ret)

        //il.Append(notBool)
        //il.Append(load0)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ldc_I4, 0)
        il.Emit(OpCodes.Ret)

    /// Create an Empty Object Constructor
    ///
    /// Creates a constructor method deifnition that just calls the parent constructor
    let private createEmptyCtor (assm: AssemblyDefinition) =
        let ctor = MethodDefinition(".ctor",
                                    MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName,
                                    assm.MainModule.TypeSystem.Void)
        let objConstructor = assm.MainModule.ImportReference(typeof<obj>.GetConstructor(Array.empty))
        let il = ctor.Body.GetILProcessor()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Call, objConstructor)
        il.Emit(OpCodes.Ret)
        ctor

    /// Emit a Bound Expression to .NET
    ///
    /// Creates an assembly and writes out the .NET interpretation of the
    /// given bound tree. This method is responsible for creating the root
    /// `LispProgram` type and preparting the emit context. The main work of
    /// lowering is done by `emitNamedLambda`.
    let emit (outputStream: Stream) outputName bound =
        // Create an assembly with a nominal version to hold our code
        let name = AssemblyNameDefinition(outputName, Version(0, 0, 0))
        let assm = AssemblyDefinition.CreateAssembly(name, "lisp_module", ModuleKind.Console)
        
        // Genreate a nominal type to contain the methods for this program.
        let progTy = TypeDefinition(outputName,
                                    "LispProgram",
                                    TypeAttributes.Class ||| TypeAttributes.Public ||| TypeAttributes.AnsiClass,
                                    assm.MainModule.TypeSystem.Object)
        assm.MainModule.Types.Add progTy
        progTy.Methods.Add <| createEmptyCtor assm

        // Emit the body of the script to a separate method so that the `Eval`
        // module can call it directly
        let rootEmitCtx = { IL = null
                          ; ProgramTy = progTy
                          ; NextLambda = 0
                          ; ScopePrefix = "$ROOT"
                          ; Assm = assm }
        let bodyMethod, _ = emitNamedLambda rootEmitCtx "$ScriptBody" [] bound

        // The `Main` method is the entry point of the program. It calls
        // `$ScriptBody` and coerces the return value to an exit code.
        let mainMethod = MethodDefinition("Main",
                                          MethodAttributes.Public ||| MethodAttributes.Static,
                                          assm.MainModule.TypeSystem.Int32)
        mainMethod.Parameters.Add(ParameterDefinition(ArrayType(assm.MainModule.TypeSystem.String)))
        progTy.Methods.Add mainMethod
        assm.EntryPoint <- mainMethod
        let il = mainMethod.Body.GetILProcessor()

        il.Emit(OpCodes.Call, bodyMethod)
        emitMainEpilogue assm il

        // Write our `Assembly` to the output stream now we are done.
        assm.Write outputStream
        assm

    /// Compile a single AST node into an assembly
    ///
    /// The plan for this is we make multiple passes over the syntax tree. First
    /// pass will be to `bind` theh tree. Resulting in a `Expression`. This will
    /// attach any type information that _can_ be computed to each node, and
    /// resolve variable references to the symbols that they refer to.
    ///
    /// Once the expression is bound we will then `emit` the expression this walks
    /// the expression and writes out the corresponding .NET IL to an `Assembly`
    /// at `outputStream`. The `outputName` controls the root namespace and assembly
    /// name of the output.
    let compile outputStream outputName =
        //let scope = createRootScope
        //bind scope node |> 
        emit outputStream outputName

    /// Read a File and Compile
    ///
    /// Takes the `path` to an input to read and compile.
    //let compileFile (path: string) =
    //    let output = Path.ChangeExtension(path, "exe")
    //    let stem = Path.GetFileNameWithoutExtension(path);
    //    parseFile path
    //    |> Result.map (fun ast ->
    //        compile (File.OpenWrite output) stem ast
    //        // TOOD: This metadata needs to be abstracted to deal with different
    //        //       target framework's prefrences. For now the `.exe` we generate
    //        //       is compatible with .NET Core and Mono. It would be nice to make
    //        //       this explicit somewhere in future.
    //        //       It would be nice to register ourselves as a proper SDK so that
    //        //       this metadata is generated for us by `dotnet`.
    //        File.WriteAllText(Path.Combine(Path.GetDirectoryName(path), stem + ".runtimeconfig.json"), """
    //        {
    //          "runtimeOptions": {
    //            "tfm": "netcoreapp3.0",
    //            "framework": {
    //              "name": "Microsoft.NETCore.App",
    //              "version": "3.0.0"
    //            }
    //          }
    //        }
    //        """))
    let eval ast =
        let memStream = new MemoryStream()
        let bytecode = compile memStream "evalCtx" ast
        let assm = System.Reflection.Assembly.Load(memStream.ToArray())
        let progTy = assm.GetType("evalCtx.LispProgram")
        // TODO: Instead of calling `$ScriptBody` here should we bind a custom
        //       function definition and call that instead? e.g.: 
        //       
        //       ```scheme
        ///      (define (evalEntry) <ast>)
        ///      ```
        //let mainMethod = progTy.GetMethod("$ScriptBody")
        //mainMethod.Invoke(null, Array.empty<obj>) |> printfn "RETURN: %A"
        //let mainMethod = progTy.GetMethod("Main")
        //mainMethod.Invoke(null, [||])

        bytecode.Write "C:/Users/mague/Source/Repos/ChimeLisp/Test.exe"
        File.WriteAllText("C:/Users/mague/Source/Repos/ChimeLisp/test.runtimeconfig.json", """
        {
          "runtimeOptions": {
            "tfm": "net8.0",
            "framework": {
              "name": "Microsoft.NETCore.App",
              "version": "8.0.0"
            }
          }
        }
        """)

        File.WriteAllText("C:/Users/mague/Source/Repos/ChimeLisp/test.deps.json", """
        {
          "runtimeTarget": {
            "name": ".NETCoreApp,Version=v8.0",
            "signature": ""
          },
          "compilationOptions": {}
        }
        """)
end