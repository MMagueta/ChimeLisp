namespace rec Language

open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic

module Matchers = begin
    let (|Native|_|) (exprs: Expression.t) =
        match exprs with
        | Expression.EList ((Expression.EAtom "+")::args) -> Some ("+", args)
        | Expression.EList ((Expression.EAtom "-")::args) -> Some ("-", args)
        | Expression.EList ((Expression.EAtom "*")::args) -> Some ("*", args)
        | Expression.EList ((Expression.EAtom "/")::args) -> Some ("/", args)
        | Expression.EList ((Expression.EAtom "=")::args) -> Some ("=", args)
        | Expression.EList ((Expression.EAtom "<")::args) -> Some ("<", args)
        | Expression.EList ((Expression.EAtom "<=")::args) -> Some ("<=", args)
        | Expression.EList ((Expression.EAtom ">")::args) -> Some (">", args)
        | Expression.EList ((Expression.EAtom ">=")::args) -> Some (">=", args)
        | _ -> None

    let (|Lambda|_|) (exprs: Expression.t list) =
        match exprs with
        | (Expression.EList ((Expression.EAtom "lambda")::(Expression.EList args)::body::[]))::rest -> Some (args, body, rest)
        | _ -> None
        
    let (|IfThenElse|_|) (exprs: Expression.t) =
        match exprs with
        | Expression.EList((Expression.EAtom "if")::condition::thenBranch::[elseBranch]) ->
            Some (condition, thenBranch, elseBranch)
        | _ -> None
        
    let (|IfThen|_|) (exprs: Expression.t) =
        match exprs with
        | Expression.EList((Expression.EAtom "if")::condition::thenBranch::[]) ->
            Some (condition, thenBranch)
        | _ -> None

    let (|Application|_|) (expr: Expression.t) =
        match expr with
        | Expression.EList (func::args) -> Some (func, args)
        | _ -> None

    let (|Quoted|_|) (expr: Expression.t) =
        match expr with
        | Expression.EList (Expression.EAtom "quote"::list) -> Some (list)
        | _ -> None
end


[<RequireQualifiedAccess>]
module Generator = begin
    open Matchers

    let codeReturnType (env: Map<_,_>) (exprs: Expression.t list) = 
        List.tryLast exprs
        |> Option.map (expressionCLRType env false)
        |> Option.defaultValue typeof<Void>
    
    let rec expressionCLRType (env : Map<string, Expression.t>) ``from application?`` expr =
        match expr with
        | Expression.EArgument (_, t) -> t
        | Expression.EAtom label ->
            match Map.tryFind label env with
            | Some value -> expressionCLRType env ``from application?`` value
            | None -> failwithf "Could not find '%s' in the environment." label
        | Expression.EIfThenElse (_, thenValue, Some elseValue) ->
            match expressionCLRType env false thenValue with
            | t when t = expressionCLRType env false elseValue -> t
            | _ -> failwith("An if statement expects the 'then' and 'else' branches to be of the same type.")
        | Expression.EIfThenElse (_, thenValue, None) ->
            expressionCLRType env false thenValue
        | Expression.EApplication (Expression.EClosure (func, None), _) -> func.ReturnType
        | Expression.EApplication (Expression.EClosure (_, Some returnType), _) -> returnType
        | Expression.EApplication (x, _) -> expressionCLRType env true x
        | Expression.EAbstraction (args, body) when ``from application?`` ->
            // TODO: Lookup types of the arguments passing down recursively form the application
            // Remove the typeof<obj>
            let (_, newEnv) = List.fold (fun (index, acc) arg -> index + 1, Map.add arg (Expression.EArgument (index, typeof<obj>)) acc) (0, env) args
            expressionCLRType newEnv false body
        | Expression.EAbstraction _ ->
            typeof<Func<_,_>>
        | Expression.ENative(_, arguments) ->
            // Gambiarra, needs to check if all are the same because of EArguments
            List.map (expressionCLRType env true) arguments
            |> List.tryPick (fun t -> if t <> typeof<Void> && t <> typeof<obj> then Some t else None)
            |> function Some t -> t
                      | None -> typeof<Void>
        //| Expression.EList (Expression.EAtom label :: args) -> invokeFromEnvironment label args env |> typeOf env
        | Expression.EList _ -> typeof<List<_>>
        | Expression.EInteger _ -> typeof<int>
        | Expression.EFloat _ -> typeof<float32>
        | Expression.EString _ -> typeof<string>
        | Expression.EVariable _ -> typeof<obj>
        | Expression.EClosure (methodBuilder, None) ->
            try 
                methodBuilder.ReturnType
            with _ -> typeof<obj>
        | Expression.EClosure (_, Some returnType) ->
            returnType
        | Expression.EProgn exprs ->
            List.tryLast exprs
            |> Option.map (expressionCLRType env false)
            |> Option.defaultValue (typeof<Void>)
        | Quoted _ -> typeof<obj>
        | otherwise -> failwithf "Not implemented: %A" otherwise
    
    let rec expand (expr: Expression.t) =
        match expr with
        | Expression.EList (Expression.EAtom "lambda" :: rest) ->
            match rest with
            | [ Expression.EList arguments; body ] ->
                Expression.EAbstraction (List.map (function Expression.EAtom label -> label) arguments, expand body)
        | IfThenElse (condition, thenBranch, elseBranch) ->
            Expression.EIfThenElse (expand condition, expand thenBranch, (Some << expand) elseBranch)
        | IfThen (condition, thenBranch) ->
            Expression.EIfThenElse (expand condition, expand thenBranch, None)
        | Native (label, args) -> Expression.ENative (label, List.map expand args)
        | Expression.EList (Expression.EAtom "defvar" :: Expression.EAtom varName::[value]) ->
            Expression.EVariable (varName, expand value)
        | Expression.EList (Expression.EAtom "progn" :: subsequents) ->
            Expression.EProgn (List.map expand subsequents)
        | Expression.EList (Expression.EAtom "defun" ::Expression.EAtom funcName::rest) ->
            match rest with
            | [Expression.EList arguments; body ] ->
                List.map (function Expression.EAtom label -> label) arguments
                |> fun labels -> (funcName, Expression.EAbstraction (labels, expand body))
                |> Expression.EVariable
        | Quoted _ as quoted -> quoted
        | Application (func, args) ->
            let expandedFunc = expand func
            let expandedArgs = List.map expand args
            Expression.EApplication (expandedFunc, expandedArgs)
        | Expression.EList elems ->
            (List.map expand >> Expression.EList) elems
        | otherwise -> otherwise


    let rec makeBlock (generator: ILGenerator) (target: TypeBuilder) env (argumentTypes: Type list option) expr: Expression.t option * ILGenerator * TypeBuilder * Map<_,_> =
        match expr with
        | (Expression.EVariable (label, value))::rest ->
            makeBlock generator target (Map.add label value env) argumentTypes rest
            
        | [Expression.EAtom label] ->
            match Map.tryFind label env with
            | Some value -> makeBlock generator target env argumentTypes [value]
            | None -> failwithf "Could not find '%s' in the environment." label

        | [Expression.EArgument (index, _)] -> 
            generator.Emit(OpCodes.Ldarg, index)
            None, generator, target, env

        | [Expression.EInteger n] -> 
            generator.Emit(OpCodes.Ldc_I4, n)
            None, generator, target, env

        | [Expression.EFloat n] -> 
            generator.Emit(OpCodes.Ldc_R4, n)
            None, generator, target, env

        | [Expression.EString s] -> 
            generator.Emit(OpCodes.Ldstr, s)
            None, generator, target, env

        | [Expression.ENative (operator, arguments)] ->
            let state = (None, generator, target, env)
            match operator with
            | "+" ->
                generator.Emit(OpCodes.Ldc_I4_0)
                let None, generator, target, _ =
                    List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                               let None, generator, target, env =
                                   makeBlock generator target env argumentTypes [arg]
                               generator.Emit(OpCodes.Add)
                               None, generator, target, env) state arguments
                None, generator, target, env
            | "-" ->
                generator.Emit(OpCodes.Ldc_I4_0)
                let None, generator, target, _ =
                    List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                               let None, generator, target, env =
                                   makeBlock generator target env argumentTypes [arg]
                               generator.Emit(OpCodes.Sub)
                               None, generator, target, env) state arguments
                None, generator, target, env
            | "*" ->
                generator.Emit(OpCodes.Ldc_I4_1)
                let None, generator, target, _ =
                    List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                               let None, generator, target, env =
                                   makeBlock generator target env argumentTypes [arg]
                               generator.Emit(OpCodes.Mul)
                               None, generator, target, env) state arguments
                None, generator, target, env
            | "/" ->
                generator.Emit(OpCodes.Ldc_I4_1)
                let None, generator, target, _ =
                    List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                               let None, generator, target, env =
                                   makeBlock generator target env argumentTypes [arg]
                               generator.Emit(OpCodes.Div)
                               None, generator, target, env) state arguments
                None, generator, target, env
            | "=" ->
                let None, generator, target, _ =
                    makeBlock generator target env argumentTypes [List.head arguments]
                let None, generator, target, _ =
                    List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                               let None, generator, target, env =
                                   makeBlock generator target env argumentTypes [arg]
                               generator.Emit(OpCodes.Ceq)
                               None, generator, target, env) state arguments
                None, generator, target, env
            | "<" ->
                // Interpreted
                let head =
                    List.head arguments
                    |> function Expression.EInteger x -> float32 x
                                    | Expression.EFloat x -> x
                List.exists (function Expression.EInteger x -> head >= (float32 x)
                                    | Expression.EFloat x -> head >= x) (List.tail arguments)
                |> function
                    | true ->
                          generator.Emit(OpCodes.Ldc_I4_0)
                    | false ->
                        generator.Emit(OpCodes.Ldc_I4_1)
                None, generator, target, env
            | "<=" ->
                // Interpreted
                let head =
                    List.head arguments
                    |> function Expression.EInteger x -> float32 x
                                    | Expression.EFloat x -> x
                List.exists (function Expression.EInteger x -> head > (float32 x)
                                    | Expression.EFloat x -> head > x) (List.tail arguments)
                |> function true -> generator.Emit(OpCodes.Ldc_I4_0)
                          | false -> generator.Emit(OpCodes.Ldc_I4_1)
                None, generator, target, env
            | ">" ->
                // Interpreted
                let head =
                    List.head arguments
                    |> function Expression.EInteger x -> float32 x
                                    | Expression.EFloat x -> x
                List.exists (function Expression.EInteger x -> head <= (float32 x)
                                    | Expression.EFloat x -> head <= x) (List.tail arguments)
                |> function true -> generator.Emit(OpCodes.Ldc_I4_0)
                          | false -> generator.Emit(OpCodes.Ldc_I4_1)
                None, generator, target, env
            | ">=" ->
                // Interpreted
                let head =
                    List.head arguments
                    |> function Expression.EInteger x -> float32 x
                                    | Expression.EFloat x -> x
                List.exists (function Expression.EInteger x -> head < (float32 x)
                                    | Expression.EFloat x -> head < x) (List.tail arguments)
                |> function true -> generator.Emit(OpCodes.Ldc_I4_0)
                          | false -> generator.Emit(OpCodes.Ldc_I4_1)
                None, generator, target, env
        | [Expression.EAbstraction (argumentNames, body)] ->
            let arguments =
                match argumentTypes with
                | Some types ->
                    // We don't need to loop over the list of argument names if they are the same
                    List.mapi (fun i type' -> Expression.EArgument (i, type')) types
                | None -> List.mapi (fun i _label -> Expression.EArgument (i, typeof<obj>)) argumentNames
            let newEnv =
                List.fold
                    (fun acc (label, argument) -> Map.add label argument acc)
                    env
                    (List.zip argumentNames arguments)
            let bodyType = codeReturnType newEnv [body]
            let lambdaMethodBuilder = 
                target.DefineMethod("Lambda",
                                    MethodAttributes.Static ||| MethodAttributes.Public,
                                    bodyType,
                                    [| for i in 0..arguments.Length - 1 do codeReturnType env [arguments.Item i] |])
            let None, closure, target, env = 
                makeBlock (lambdaMethodBuilder.GetILGenerator()) target newEnv argumentTypes [body]
            closure.Emit(OpCodes.Ret)
            Some <| Expression.EClosure (lambdaMethodBuilder, Some bodyType), generator, target, env

        | [Quoted _ as quoted] ->
            Some quoted, generator, target, env
            
        | [Expression.EList (x::rest)] ->
            let clone = generator
            let head, generator, target, _ =
                makeBlock generator target env argumentTypes [x]
            match head with
            | Some (Expression.EClosure _ as closure) ->
                makeBlock generator target env argumentTypes [Expression.EApplication (closure, rest)]
            | Some otherwise -> makeBlock generator target env argumentTypes [Expression.EList (otherwise::rest)]
            | None ->
                let listType = codeReturnType env [x]
                generator.Emit(OpCodes.Newarr, listType)
                Some (Expression.EList (x::rest)), generator, target, env
                // makeBlock generator target env argumentTypes [Expression.EList (otherwise::rest)]

        | [Expression.EApplication (func, args)] ->
            // TODO: Merge the environments later
            let Some (Expression.EClosure (methodBuilder, _)), _, target, newEnv =
                makeBlock generator target env (Some (List.map (fun x -> codeReturnType env [x]) args)) [func]
            let None, generator, target, _ =
                List.fold (fun (None, generator, target, _)
                               argumentToCompile ->
                        makeBlock generator target env argumentTypes [argumentToCompile])
                        (None, generator, target, newEnv)
                        args
            generator.Emit(OpCodes.Call, methodBuilder)

            None, generator, target, newEnv
        | [Expression.EClosure _ as closure] ->
            Some closure, generator, target, env

        | [Expression.EIfThenElse (condition, thenBranch, Some elseBranch)] ->
            let elseLabel = generator.DefineLabel()
            let endLabel = generator.DefineLabel()
            let None, generator, target, _ = makeBlock generator target env argumentTypes [condition]
            generator.Emit(OpCodes.Brfalse, elseLabel)
            let None, generator, target, _ = makeBlock generator target env argumentTypes [thenBranch]
            generator.Emit(OpCodes.Br, endLabel)
            generator.MarkLabel(elseLabel)
            let None, generator, target, _ = makeBlock generator target env argumentTypes [elseBranch]
            generator.MarkLabel(endLabel)
            None, generator, target, env

        | [Expression.EIfThenElse (condition, thenBranch, None)] ->
            let endLabel = generator.DefineLabel()
            let None, generator, target, _ = makeBlock generator target env argumentTypes [condition]
            generator.Emit(OpCodes.Brfalse, endLabel)
            let None, generator, target, _ = makeBlock generator target env argumentTypes [thenBranch]
            generator.MarkLabel(endLabel)
            None, generator, target, env
            
        | [Expression.EProgn exprs] ->
            let None, generator, target, _ =
                List.fold (fun (None, generator, target, _) expr ->
                    makeBlock generator target env argumentTypes [expr]) (None, generator, target, env) exprs
            None, generator, target, env

        | otherwise -> printfn "------------OTHERWISE------------\n%A" otherwise; printfn "------------OTHERWISE------------"; failwith "Not implemented"

    let compile generator target ``is interactive call?`` exprs =
        let prelude =
            Map.empty
            |> Map.add "println" (Expression.EClosure (typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> |]), Some typeof<Void>))
            |> Map.add "print" (Expression.EClosure (typeof<System.Console>.GetMethod("Write", [| typeof<string> |]), Some typeof<Void>))
            |> Map.add "int->string" (Expression.EClosure (typeof<System.Convert>.GetMethod("ToString", [| typeof<int> |]), Some typeof<string>))
            |> Map.add "float->string" (Expression.EClosure (typeof<System.Convert>.GetMethod("ToString", [| typeof<float32> |]), Some typeof<string>))

        // printfn "%A" exprs
        
        let _, generator, target, finalEnv = makeBlock generator target prelude None exprs
        // TODO: Right now final env is required because of definitions not being present in the prelude
        let lastType = codeReturnType finalEnv exprs
        if (lastType.IsValueType || lastType = typeof<string>) && lastType <> typeof<Void>
        then
            if ``is interactive call?`` then
                generator.Emit(OpCodes.Call, typeof<System.Console>.GetMethod("WriteLine", [| lastType |]))
            else generator.Emit(OpCodes.Pop)
        // for now lists are not compiled with Newarr
        else if lastType = typeof<List<_>> && ``is interactive call?`` then (List.tryLast exprs |> Option.map (printfn "%A")) |> ignore
        generator.Emit(OpCodes.Ldc_I4, 0)
        generator.Emit(OpCodes.Ret)
        target, finalEnv

    let wrapper ``is interactive call?`` exprs: int * Map<_,_> =
        let assembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("ChimeLisp"), AssemblyBuilderAccess.Run)
        let moduleBuilder = assembly.DefineDynamicModule("ChimeLisp")
        let typeBuilder = moduleBuilder.DefineType("ChimeLisp", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let entry = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>; typeof<string array>|])
        let target, finalEnv = compile (entry.GetILGenerator()) typeBuilder ``is interactive call?`` exprs
        target.CreateType().GetMethod("Main").Invoke((), [|0; ([||]: string array)|]) :?> int, finalEnv
        
end

[<RequireQualifiedAccess>]
module Expression = begin
    
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
    | EClosure of MethodInfo * ReturnType: Type option
    | ENative of string * t list
    | EIfThenElse of t * t * t option
    // | EVariableReference of LocalBuilder
    | EProgn of t list
end
