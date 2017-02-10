[<AutoOpen>]
module internal FsCheck.Async.Utils

open System
open System.Threading
open System.Threading.Tasks

let (|Basic|Generic|Array|Pointer|ByRef|) (t : Type) =
    if t.IsGenericType then
        Generic(t.GetGenericTypeDefinition(), t.GetGenericArguments())
    elif t.IsArray then
        Array (t.GetElementType())
    elif t.IsPointer then
        Pointer(t.GetElementType())
    elif t.IsByRef then
        ByRef(t.GetElementType())
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

type ThrottledNonDeterministicRunner<'T>(degreeOfParallelism : int, ctoken : CancellationToken) =
    do if degreeOfParallelism < 1 then invalidArg "degreeOfParallelism" "must be nonzero value"
    let semaphore = new SemaphoreSlim(degreeOfParallelism)
    let resultHolder = new TaskCompletionSource<'T>()

    // NB: task must be exception safe or we risk killing the process!
    member __.Enqueue(task : Async<'T option>) = async {
        do! semaphore.WaitAsync ctoken |> Async.AwaitTaskCorrect
        let worker() = async {
            try
                let! result = task
                match result with
                | Some t -> resultHolder.TrySetResult t |> ignore
                | None -> ()
            finally
                semaphore.Release() |> ignore
        }

        Async.Start(worker(), ctoken)
        return ()
    }

    member __.IsCompleted = resultHolder.Task.IsCompleted

    member __.AwaitCompletion() = async {
        // ensure all child tasks have completed by
        // claiming the entire semaphore capacity
        // for the current workflow
        for _ in 1 .. degreeOfParallelism do
            do! semaphore.WaitAsync ctoken |> Async.AwaitTaskCorrect

        if resultHolder.Task.IsCompleted then
            return Some resultHolder.Task.Result
        else
            return None
    }

    interface IDisposable with
        member __.Dispose() = semaphore.Dispose()