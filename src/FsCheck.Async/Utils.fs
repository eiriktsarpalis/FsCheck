[<AutoOpen>]
module internal FsCheck.Async.Utils

open System
open System.Threading
open System.Threading.Tasks

let (|Basic|Generic|Array|) (t : Type) =
    if t.IsGenericType then
        Generic(t.GetGenericTypeDefinition(), t.GetGenericArguments())
    elif t.IsArray then
        Array (t.GetElementType())
    else
        Basic t

let (|Bool|_|) (t : 'T) =
    match box t with :? bool as b -> Some b | _ -> None

type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R

[<AbstractClass>]
type TypeShape() =
    abstract Type : Type
    abstract Accept : IFunc<'R> -> 'R

type TypeShape<'T>() =
    inherit TypeShape()
    override __.Type = typeof<'T>
    override __.Accept f = f.Invoke<'T> ()

type TypeShape with
    static member Create(t : Type) =
        let shape = typedefof<TypeShape<_>>.MakeGenericType [|t|]
        Activator.CreateInstance shape :?> TypeShape

[<RequireQualifiedAccess>]
module Seq =

    let takeAtMost (n : int) (ts : seq<'T>) = seq {
        use enum = ts.GetEnumerator()
        let count = ref 0
        while !count < n && enum.MoveNext() do
            yield enum.Current
            incr count
    }

type private Latch() =
    let mutable counter = 0
    member inline __.Enter() = Interlocked.Increment &counter = 1

type Async with

    static member AwaitTaskCorrect (task : Task<'T>) : Async<'T> = async {
        let! ct = Async.CancellationToken
        return! Async.FromContinuations(fun (sc,ec,cc) ->
            let l = new Latch()
            let ctrDisposer = ct.Register(fun _ -> if l.Enter() then cc(OperationCanceledException()))
            task.ContinueWith(fun (task:Task<'T>) ->
                if l.Enter() then
                    ctrDisposer.Dispose()
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                        else ec e
                    elif task.IsCanceled then
                        ec(TaskCanceledException())
                    else
                        sc task.Result)
            |> ignore)
    }

    static member AwaitTaskCorrect(task : Task) : Async<unit> = async {
        let! ct = Async.CancellationToken
        return! Async.FromContinuations(fun (sc,ec,cc) ->
            let l = new Latch()
            let ctrDisposer = ct.Register(fun _ -> if l.Enter() then cc(OperationCanceledException()))
            task.ContinueWith(fun (task:Task) ->
                if l.Enter() then
                    ctrDisposer.Dispose()
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                        else ec e
                    elif task.IsCanceled then
                        ec(TaskCanceledException())
                    else
                        sc ())
            |> ignore)
    }

type ThrottledNonDeterministicRunner<'T>(degreeOfParallelism : int) =
    do if degreeOfParallelism < 1 then invalidArg "degreeOfParallelism" "must be nonzero value"
    let cts = new CancellationTokenSource()
    let semaphore = new SemaphoreSlim(degreeOfParallelism)
    let resultHolder = new TaskCompletionSource<'T>()
    let mutable numTasks = 0

    member __.Enqueue(task : Async<'T option>) = async {
        let! ct = Async.CancellationToken
        let cts' = CancellationTokenSource.CreateLinkedTokenSource(ct, cts.Token)
        do! semaphore.WaitAsync ct |> Async.AwaitTaskCorrect
        let _ = Interlocked.Increment &numTasks
        let worker() = async {
            try
                try
                    let! result = task
                    match result with
                    | Some t -> resultHolder.TrySetResult t |> ignore
                    | None -> ()
                finally
                    semaphore.Release() |> ignore
            with _ -> () // workflows passed to Async.Start need to be exception safe
                         // otherwise we risk killing the process
        }

        Async.Start(worker(), cts'.Token)
        return ()
    }

    member __.IsCompleted = resultHolder.Task.IsCompleted
    member __.NumberOfTasks = numTasks

    member __.AwaitCompletion() = async {
        let! ct = Async.CancellationToken
        for _ in 1 .. degreeOfParallelism do
            do! semaphore.WaitAsync ct |> Async.AwaitTaskCorrect

        if resultHolder.Task.IsCompleted then
            return Some resultHolder.Task.Result
        else
            return None
    }

    interface IDisposable with
        member __.Dispose() = semaphore.Dispose() ; cts.Dispose()