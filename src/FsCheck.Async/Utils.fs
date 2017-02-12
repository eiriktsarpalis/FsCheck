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

type Async with

    static member AwaitTaskCorrect (task : Task<'T>) : Async<'T> =
        Async.FromContinuations(fun (sc,ec,_) ->
            task.ContinueWith(fun (task:Task<'T>) ->
                if task.IsFaulted then
                    let e = task.Exception
                    if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                    else ec e
                elif task.IsCanceled then
                    ec(TaskCanceledException())
                else
                    sc task.Result)
            |> ignore)

    static member AwaitTaskCorrect(task : Task) : Async<unit> =
        Async.FromContinuations(fun (sc,ec,_) ->
            task.ContinueWith(fun (task:Task) ->
                if task.IsFaulted then
                    let e = task.Exception
                    if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                    else ec e
                elif task.IsCanceled then
                    ec(TaskCanceledException())
                else
                    sc ())
            |> ignore)

    // This is the main driver for async/parallel property test runs
    // takes a sequence of nondeterministic async workflows, running at most
    // `degreeOfParallelism` items in parallel at a given moment
    // Once a task completing with `Some t` is discovered, it will cause the
    // overall workflow to complete with that result.
    static member ChoiceThrottled (degreeOfParallelism : int) (tasks : seq<Async<'T option>>) : Async<'T option> = async {
        do if degreeOfParallelism < 1 then invalidArg "degreeOfParallelism" "must be positive value"
        let! ct = Async.CancellationToken
        use cts = CancellationTokenSource.CreateLinkedTokenSource(ct)
        let tcs = new TaskCompletionSource<'T>()
        let isTcsUnset() = not (tcs.Task.IsCompleted || tcs.Task.IsFaulted)
        use semaphore = new SemaphoreSlim(degreeOfParallelism)

        use enum = tasks.GetEnumerator()
        while enum.MoveNext() && isTcsUnset() do
            do! semaphore.WaitAsync ct |> Async.AwaitTaskCorrect
            let item = enum.Current

            let wrapper = async {
                let! result = Async.Catch item
                match result with
                | Choice1Of2 None -> ()
                | Choice1Of2 (Some t) ->
                    let _ = tcs.TrySetResult t
                    cts.Cancel()
                | Choice2Of2 e ->
                    let _ = tcs.TrySetException e
                    cts.Cancel()
            }

            // start item task and ensure that semaphore 
            // will *always* be released on task completion
            let task = Async.StartAsTask(wrapper, cancellationToken = cts.Token)
            let _ = task.ContinueWith(fun (_:Task<_>) -> semaphore.Release())
            ()

        // ensure all child tasks have completed by
        // claiming the entire semaphore capacity
        for _ in 1 .. degreeOfParallelism do
            do! semaphore.WaitAsync ct |> Async.AwaitTaskCorrect

        if isTcsUnset() then return None
        else 
            let! result = Async.AwaitTaskCorrect tcs.Task
            return Some result
    }