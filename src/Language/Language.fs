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
        | Expression.EArgument _ -> typeof<Void>
        | Expression.EAtom label ->
            match Map.tryFind label env with
            | Some value -> expressionCLRType env false value
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
            let (_, newEnv) = List.fold (fun (index, acc) arg -> index + 1, Map.add arg (Expression.EArgument index) acc) (0, env) args
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
        | Expression.EList (Expression.EAtom "defun" ::Expression.EAtom funcName::rest) ->
            match rest with
            | [Expression.EList arguments; body ] ->
                List.map (function Expression.EAtom label -> label) arguments
                |> fun labels -> (funcName, Expression.EAbstraction (labels, expand body))
                |> Expression.EVariable
        | Application (func, args) ->
            let expandedFunc = expand func
            let expandedArgs = List.map expand args
            Expression.EApplication (expandedFunc, expandedArgs)
        | Expression.EList elems ->
            (List.map expand >> Expression.EList) elems
        | otherwise -> otherwise


    let rec makeBlock (generator: ILGenerator) (target: TypeBuilder) env expr: Expression.t option * ILGenerator * TypeBuilder * Map<_,_> =
        match expr with
        | (Expression.EVariable (label, value))::rest ->
            makeBlock generator target (Map.add label value env) rest
            
        | [Expression.EAtom label] ->
            match Map.tryFind label env with
            | Some value -> makeBlock generator target env [value]
            | None -> failwithf "Could not find '%s' in the environment." label

        | [Expression.EArgument index] -> 
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
            let None, generator, target, _ =
                List.fold (fun (None, generator, target, _) (arg: Expression.t) ->
                    makeBlock generator target env [arg]) state arguments
            match operator with
            | "+" -> generator.Emit(OpCodes.Add); None, generator, target, env
            | "=" -> generator.Emit(OpCodes.Ceq); None, generator, target, env
        
        | [Expression.EAbstraction (argumentNames, body)] ->
            let arguments = List.mapi (fun i _label -> Expression.EArgument i) argumentNames
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
                                    [| for _ in 1..arguments.Length do typeof<int> |])
            let None, closure, target, env = 
                makeBlock (lambdaMethodBuilder.GetILGenerator()) target newEnv [body]
            closure.Emit(OpCodes.Ret)
            Some <| Expression.EClosure (lambdaMethodBuilder, Some bodyType), generator, target, env
            
        | [Expression.EList (x::rest)] ->
            makeBlock generator target env [Expression.EApplication (x, rest)]

        | [Expression.EApplication (func, args)] ->
            // TODO: Merge the environments later
            let Some (Expression.EClosure (methodBuilder, _)), _, target, newEnv =
                makeBlock generator target env [func]
            let None, generator, target, _ =
                List.fold (fun (None, generator, target, _)
                               argumentToCompile ->
                        makeBlock generator target env [argumentToCompile])
                        (None, generator, target, newEnv)
                        args
            generator.Emit(OpCodes.Call, methodBuilder)

            None, generator, target, newEnv
        | [Expression.EClosure _ as closure] ->
            Some closure, generator, target, env

        | [Expression.EIfThenElse (condition, thenBranch, Some elseBranch)] ->
            let elseLabel = generator.DefineLabel()
            let endLabel = generator.DefineLabel()
            let None, generator, target, _ = makeBlock generator target env [condition]
            generator.Emit(OpCodes.Brfalse, elseLabel)
            let None, generator, target, _ = makeBlock generator target env [thenBranch]
            generator.Emit(OpCodes.Br, endLabel)
            generator.MarkLabel(elseLabel)
            let None, generator, target, _ = makeBlock generator target env [elseBranch]
            generator.MarkLabel(endLabel)
            None, generator, target, env

        | [Expression.EIfThenElse (condition, thenBranch, None)] ->
            let endLabel = generator.DefineLabel()
            let None, generator, target, _ = makeBlock generator target env [condition]
            generator.Emit(OpCodes.Brfalse, endLabel)
            let None, generator, target, _ = makeBlock generator target env [thenBranch]
            generator.MarkLabel(endLabel)
            None, generator, target, env
            
        | otherwise -> printfn "------------OTHERWISE------------\n%A" otherwise; printfn "------------OTHERWISE------------"; failwith "Not implemented"

    let compile generator target exprs =
        let prelude =
            Map.empty
            |> Map.add "println" (Expression.EClosure (typeof<System.Console>.GetMethod("WriteLine", [| typeof<string> |]), Some typeof<Void>))
            |> Map.add "print" (Expression.EClosure (typeof<System.Console>.GetMethod("Write", [| typeof<string> |]), Some typeof<Void>))
            |> Map.add "int->string" (Expression.EClosure (typeof<System.Convert>.GetMethod("ToString", [| typeof<int> |]), Some typeof<string>))
            |> Map.add "float->string" (Expression.EClosure (typeof<System.Convert>.GetMethod("ToString", [| typeof<float32> |]), Some typeof<string>))
        
        printfn "%A" exprs
        
        let _, generator, target, finalEnv = makeBlock generator target prelude exprs
        // TODO: Right now final env is required because of definitions not being present in the prelude
        let lastType = codeReturnType finalEnv exprs
        if lastType.IsValueType && lastType <> typeof<Void>
        then generator.Emit(OpCodes.Pop)
        generator.Emit(OpCodes.Ldc_I4, 0)
        generator.Emit(OpCodes.Ret)
        target, finalEnv

    let wrapper exprs =
        
        let assembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("ChimeLisp"), AssemblyBuilderAccess.Run)
        let moduleBuilder = assembly.DefineDynamicModule("ChimeLisp")
        let typeBuilder = moduleBuilder.DefineType("ChimeLisp", TypeAttributes.Sealed ||| TypeAttributes.Public)
        let entry = typeBuilder.DefineMethod("Main", MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<int>; typeof<string array>|])
        let target, finalEnv = compile (entry.GetILGenerator()) typeBuilder exprs
        target.CreateType().GetMethod("Main").Invoke((), [|0; ([||]: string array)|]), finalEnv
        
end

[<RequireQualifiedAccess>]
module Expression = begin
    
    type t =
    | EAtom of string
    | EArgument of Index: int
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
    | EVariableReference of LocalBuilder
end
