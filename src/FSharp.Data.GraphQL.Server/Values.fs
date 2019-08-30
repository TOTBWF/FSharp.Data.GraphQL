/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

[<AutoOpen>]
module internal FSharp.Data.GraphQL.Values

open System
open System.Reflection
open System.Collections.Generic
open FSharp.Data.GraphQL.Ast
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Types.Patterns

/// Tries to convert type defined in AST into one of the type defs known in schema.
let inline tryConvertAst schema ast =
    let rec convert isNullable (schema: ISchema) (ast: InputType) : TypeDef option =
        match ast with
        | NamedType name ->
            match schema.TryFindType name with
            | Some namedDef ->
                Some (if isNullable then upcast namedDef.MakeNullable() else upcast namedDef)
            | None -> None
        | ListType inner ->
            convert true schema inner
            |> Option.map (fun i ->
                if isNullable
                then upcast i.MakeList().MakeNullable()
                else upcast i.MakeList())
        | NonNullType inner ->
            convert false schema inner
    convert true schema ast

let inline private notAssignableMsg (innerDef: InputDef) value : string =
    sprintf "value of type %s is not assignable from %s" innerDef.Type.Name (value.GetType().Name)

let rec internal compileByType (errMsg: string) (inputDef: InputDef): ExecuteInput =
    match inputDef with
    | Scalar scalardef ->
        variableOrElse scalardef.CoerceInput
    | InputObject objdef ->
        let objtype = objdef.Type
        let ctor = ReflectionHelper.matchConstructor objtype (objdef.Fields |> Array.map (fun x -> x.Name))
        // Map constructor parameters to object field definitions
        let fieldDefs =
            ctor.GetParameters()
            |> Array.map(fun param ->
                match objdef.Fields |> Array.tryFind(fun field -> field.Name = param.Name) with
                | Some x -> x
                | None ->
                    failwithf "Input object '%s' refers to type '%O', but constructor parameter '%s' doesn't match any of the defined input fields" objdef.Name objtype param.Name)

        let rec compileObject (value : Value) (variables : Map<string, Value>) (metadata : Metadata) =
            match value with
            | ObjectValue props ->
                let args =
                    fieldDefs
                    |> Array.map (fun field ->
                        match Map.tryFind field.Name props with
                        | None -> null
                        | Some prop -> field.ExecuteInput prop variables metadata)
                let instance = ctor.Invoke(args)
                instance
            | Variable variableName ->
                match Map.tryFind variableName variables with
                | Some varValue -> compileObject varValue variables metadata
                | None -> null
            | NullValue -> null
            | _ -> failwithf "Expected an input object '%s', but recieved '%A'" objdef.Name value

        fun value variables metadata -> compileObject value variables metadata
    | List (Input innerdef) ->
        let inner = compileByType errMsg innerdef
        let cons, nil = ReflectionHelper.listOfType innerdef.Type

        let rec compileList (value : Value) (variables : Map<string, Value>) (metadata : Metadata) =
            match value with
            | ListValue list ->
                let mappedValues = list |> List.map (fun value -> inner value variables metadata)
                nil |> List.foldBack cons mappedValues
            | Variable variableName ->
                match Map.tryFind variableName variables with
                | Some varValue -> compileList varValue variables metadata
                | None -> null
            | _ ->
                // try to construct a list from single element
                let single = inner value variables metadata
                if single = null then null else cons single nil

        fun value variables metadata -> compileList value variables metadata
    | Nullable (Input innerdef) ->
        let inner = compileByType errMsg innerdef
        let some, none = ReflectionHelper.optionOfType innerdef.Type

        fun variables value metadata ->
            let i = inner variables value metadata
            match i with
            | null -> none
            | coerced ->
                let c = some coerced
                if c <> null then c
                else raise(GraphQLException (errMsg + notAssignableMsg innerdef coerced))
    | Enum enumdef ->
        let compileEnum value metadata =
            let coerced = coerceEnumInput metadata value
            match coerced with
            | None -> null
            | Some s -> ReflectionHelper.parseUnion enumdef.Type s

        fun value variables metadata ->
            match value with
            | Variable variableName ->
                match variables.TryFind variableName with
                | Some varValue -> compileEnum varValue metadata
                | None -> failwithf "Variable '%s' not supplied.\nVariables: %A" variableName variables
            | _ -> compileEnum value metadata
    | _ -> failwithf "Unexpected value of inputDef: %O" inputDef
