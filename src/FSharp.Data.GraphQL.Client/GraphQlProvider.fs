/// The MIT License (MIT)
/// Copyright (c) 2016 Bazinga Technologies Inc

namespace FSharp.Data.GraphQL.Client

open System
open System.IO
open System.Net
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices

open FSharp.Data.GraphQL.Types.Introspection
open TypeCompiler

type internal ProviderSchemaConfig =
    { Namespace: string 
      DefinedTypes: Map<string, ProvidedTypeDefinition option> }

//[<TypeProvider>]
type GraphQlProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    
    let asm = System.Reflection.Assembly.GetExecutingAssembly()

    let requestSchema (url: string) =
        async {
            let requestUrl = Uri(Uri(url), ("/?query=" + FSharp.Data.GraphQL.Introspection.introspectionQuery), false)
            let req = WebRequest.CreateHttp(requestUrl)
            req.Method <- "GET"
            use! resp = req.GetResponseAsync() |> Async.AwaitTask
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
            let! json = reader.ReadToEndAsync() |> Async.AwaitTask
            let result = Serialization.fromJson json
            match result.Errors with
            | None ->
                let introspectionSchema = result.Data.__schema
                return Choice1Of2 introspectionSchema
            | Some errors ->
                return Choice2Of2 errors
        }

    let compileTypesFromSchema ns (schema: IntrospectionSchema) = 
        let ctx = {
            Assembly = asm
            Namespace = ns
            KnownTypes = ProviderSessionContext.CoreTypes }
        let typeDefinitions =
            schema.Types
            |> Array.fold (fun acc t -> Map.add t.Name (ProvidedType (initType ctx t, t)) acc) ctx.KnownTypes
        let defctx = { ctx with KnownTypes = typeDefinitions }
        typeDefinitions
        |> Map.iter (fun k t -> 
            match t with
            | NativeType _ -> ()
            | ProvidedType (t, itype) -> genType defctx itype t)
        typeDefinitions
        |> Map.toSeq 
        |> Seq.map snd
        |> Seq.choose (
            function
            | NativeType _ -> None
            | ProvidedType (t, _) -> Some t)
        |> Seq.toList

    do
        let choice = requestSchema("http://localhost:8083") |> Async.RunSynchronously
        match choice with
        | Choice1Of2 schema ->
            let types = compileTypesFromSchema "GraphQLNamespace" schema
            this.AddNamespace("GraphQLNamespace", types)
        | Choice2Of2 ex -> () 


[<assembly:TypeProviderAssembly>]
do ()