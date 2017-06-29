﻿namespace FSharp.Data.GraphQL

open System
open System.Collections.Generic

/// <summary>
/// A struct used to operate on both synchronous values and Async computations
/// using the same, uniform API.
/// </summary>
type AsyncVal<'T> =
    | Value of 'T
    | Async of Async<'T>
    | FailedWith of Exception

    static member Zero = Value(Unchecked.defaultof<'T>)
    override x.ToString () = 
        match x with
        | Value v -> "AsyncVal(" + v.ToString() + ")"
        | Async a -> "AsyncVal(Async<>)"
        | FailedWith f -> "AsyncVal(Failure:" + f.Message + ")"
    
[<RequireQualifiedAccess>]
module AsyncVal =
    
    /// Returns true if AsyncVal wraps an Async computation, otherwise false.
    let inline isAsync (x: AsyncVal<'T>) = match x with | Async _ -> true | _ -> false

    /// Returns true if AsyncVal contains immediate result, otherwise false.
    let inline isSync (x: AsyncVal<'T>) = match x with | Value _ -> true | _ -> false

    /// Returns true if the AsyncVal failed, otherwise false
    let inline isFailure (x: AsyncVal<'T>) = match x with | FailedWith _ -> true | _ -> false

    /// Returns value wrapped by current AsyncVal. If it's part of Async computation,
    /// it's executed synchronously and then value is returned.
    /// If the asyncVal failed, then the exception that caused the failure is raised
    let get (x: AsyncVal<'T>) = 
        match x with
        | Value v -> v
        | Async a -> a |> Async.RunSynchronously
        | FailedWith f -> raise f

    /// Create new AsyncVal from Async computation.
    let inline ofAsync (a: Async<'T>) = Async(a)

    /// Returns an AsyncVal wrapper around provided Async computation.
    let inline wrap (v: 'T) = Value(v)
    
    /// Converts AsyncVal to Async computation.
    let toAsync (x: AsyncVal<'T>) =
        match x with
        | Value v -> async.Return v
        | Async a -> a
        | FailedWith f -> async.Return (raise f)

    /// Returns an empty AsyncVal with immediatelly executed value.
    let inline empty<'T> : AsyncVal<'T> = AsyncVal<'T>.Zero

    /// Maps content of AsyncVal using provided mapping function, returning new 
    /// AsyncVal as the result.
    let map (fn: 'T -> 'U) (x: AsyncVal<'T>) =
        match x with
        | Value v -> Value(fn v)
        | Async a -> 
            Async(async {
                let! result = a
                return fn result
            })
        | FailedWith f -> FailedWith(f)


    /// Applies rescue fn in case when contained Async value throws an exception.
    let rescue (fn: exn -> 'T) (x: AsyncVal<'T>) =
        match x with
        | Value v -> Value(v)
        | Async a ->
            Async(async {
                try return! a
                with e -> return fn e
            })
        | FailedWith f -> Value(fn f)


    /// Folds content of AsyncVal over provided initial state zero using provided fn.
    /// Returns new AsyncVal as a result.
    let fold (fn: 'State -> 'T -> 'State) (zero: 'State) (x: AsyncVal<'T>) : AsyncVal<'State> =
        match x with
        | Value v -> Value(fn zero v)
        | Async a -> 
            Async(async{
                let! res = a
                return fn zero res
            })
        | FailedWith f -> FailedWith(f)


    /// Binds AsyncVal using binder function to produce new AsyncVal.
    let bind (binder: 'T -> AsyncVal<'U>) (x: AsyncVal<'T>) : AsyncVal<'U> =
        match x with
        | Value v -> binder v
        | Async a -> 
            Async(async{
                let! value = a
                let bound = binder value
                match bound with
                | Value v -> return v
                | Async a -> return! a
                | FailedWith f -> return raise f
            })
        | FailedWith f -> FailedWith(f)
            
    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed asynchronously, one by one with regard to their order in array.
    /// Returned array maintain order of values.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectSequential (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then Value [||]
        elif values |> Array.exists isAsync then
            Async(async {
                let results = Array.zeroCreate values.Length
                for i = 0 to values.Length - 1 do
                    let v = values.[i]
                    match v with
                    | Value v -> results.[i] <- v
                    | Async a ->
                        let! r = a
                        results.[i] <- r
                    | FailedWith f -> 
                        results.[i] <- raise f
                return results })
        else Value (values |> Array.map (fun (Value v) -> v))

            

    /// Converts array of AsyncVals into AsyncVal with array results.
    /// In case when are non-immediate values in provided array, they are 
    /// executed all in parallel, in unordered fashion. Order of values
    /// inside returned array is maintained.
    /// If the array contains a Failure, then the entire array will not resolve
    let collectParallel (values: AsyncVal<'T> []) : AsyncVal<'T []> =
        if values.Length = 0 then Value [||]
        else
            let indexes = List<_>(0)
            let continuations = List<_>(0)
            let results = Array.zeroCreate values.Length
            for i = 0 to values.Length - 1 do
                let value = values.[i]
                match value with
                | Value v -> results.[i] <- v
                | Async a ->
                    indexes.Add i
                    continuations.Add a
                | FailedWith f ->
                    results.[i] <- raise f
            if indexes.Count = 0
            then Value(results)
            else Async(async {
                let! vals = continuations |> Async.Parallel
                for i = 0 to indexes.Count - 1 do
                    results.[indexes.[i]] <- vals.[i]
                return results })

type AsyncValBuilder () =
    member x.Zero () = AsyncVal.empty
    member x.Return v = AsyncVal.wrap v
    member x.ReturnFrom (v: AsyncVal<_>) = v
    member x.ReturnFrom (a: Async<_>) = AsyncVal.ofAsync a
    member x.Bind (v: AsyncVal<'T>, binder: 'T -> AsyncVal<'U>) = 
        AsyncVal.bind binder v
    member x.Bind (a: Async<'T>, binder: 'T -> AsyncVal<'U>) = 
        Async(async {
            let! value = a
            let bound = binder value
            match bound with
            | Value v -> return v
            | Async a -> return! a 
            | FailedWith f -> return raise f })

            
[<AutoOpen>]
module AsyncExtensions =
    
    /// Computation expression for working on AsyncVals.
    let asyncVal = AsyncValBuilder ()
    
    /// Active pattern used for checking if AsyncVal contains immediate value.
    let (|Immediate|_|) (x: AsyncVal<'T>) = match x with | Value v -> Some v | _ -> None

    /// Active patter used for checking if AsyncVal wraps an Async computation.
    let (|Async|_|) (x: AsyncVal<'T>) = match x with | Async a -> Some a | _ -> None

    type Microsoft.FSharp.Control.AsyncBuilder with

        member x.ReturnFrom (v: AsyncVal<'T>) =
            match v with
            | Value v -> async.Return v
            | Async a -> async.ReturnFrom a
            | FailedWith f -> async.Return (raise f)

        member x.Bind (v: AsyncVal<'T>, binder) =
            match v with
            | Value v -> async.Bind(async.Return v, binder)
            | Async a -> async.Bind(a, binder)
            | FailedWith f -> async.Bind(async.Return (raise f), binder)