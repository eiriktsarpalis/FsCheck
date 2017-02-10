[<AutoOpen>]
module internal FsCheck.Async.Runner

open System
open System.Reflection
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks

open Microsoft.FSharp.Reflection

open FsCheck
open FsCheck.Random
open FsCheck.Async

type CounterExample<'T> with
    member __.SmallestValue =
        match __.Shrinks with
        | (v,_) :: _ -> v
        | [] -> failwithf "internal error"

    member __.ToUntyped =
        { StdGen = __.StdGen ; Size = __.Size ; 
          Shrinks = __.Shrinks |> List.map (fun (t,s) -> sprintf "%A" t, s) }

let runRandomTests (config : AsyncConfig) (arb : Arbitrary<'T>) (property : AsyncProperty<'T>) = async {
    let seed = match config.Replay with None -> newSeed() | Some s -> s
    let initSize = float config.StartSize
    let increaseSizeStep = float (config.EndSize - config.StartSize) / float config.MaxTest
    let resize sz = sz + increaseSizeStep

    // generate the random items required for the test run
    let rec genSeeds (size : float) (rnd0 : StdGen) = seq {
        let rnd1, rnd2 = Random.split rnd0
        yield int(round size), rnd2
        yield! genSeeds (resize size) rnd1
    }

    let randomItems = genSeeds initSize seed |> Seq.take config.MaxTest

    // perform the async test run
    use runner = new ThrottledNonDeterministicRunner<CounterExample<'T>>(config.DegreeOfParallelism)
    use enum = randomItems.GetEnumerator()
    while enum.MoveNext() && not runner.IsCompleted do
        let size, stdGen = enum.Current
        let instanceRunner = async {
            let value = Gen.eval size stdGen arb.Generator
            let! outcome = property value
            match outcome with
            | Completed -> return None
            | Falsified
            | Exception _ -> return Some { StdGen = stdGen ; Size = size ; Shrinks = [(value, outcome)] }
        }

        do! runner.Enqueue instanceRunner

    return! runner.AwaitCompletion()
}

let rec runShrinker (config : AsyncConfig)
                    (arb : Arbitrary<'T>) (property : AsyncProperty<'T>)
                    (maxShrinks : int) (shrink : CounterExample<'T>) = async {

    if maxShrinks <= 0 then return shrink else

    let shrinkCandidates = arb.Shrinker shrink.SmallestValue |> Seq.takeAtMost maxShrinks
    let! shrinkSearchResult, numberOfRuns = async {
        use runner = new ThrottledNonDeterministicRunner<CounterExample<'T>>(config.DegreeOfParallelism)
        use enum = shrinkCandidates.GetEnumerator()
        while enum.MoveNext() && not runner.IsCompleted do
            let shrinkValue = enum.Current
            let instanceRunner = async {
                let! outcome = property shrinkValue
                match outcome with
                | Completed -> return None
                | Falsified
                | Exception _ -> return Some { shrink with Shrinks = (shrinkValue, outcome) :: shrink.Shrinks }
            }

            do! runner.Enqueue instanceRunner

        let! result = runner.AwaitCompletion()
        return result, runner.NumberOfTasks
    }

    match shrinkSearchResult with
    | None -> return shrink
    | Some result -> return! runShrinker config arb property (maxShrinks - numberOfRuns) result
}

let runPropertyTestAsync (config : AsyncConfig option) (arb : Arbitrary<'T> option) (property : AsyncProperty<'T>) = async {
    let arb = match arb with Some a -> a | None -> Arb.from<'T>
    let config = match config with Some c -> c | None -> AsyncConfig.Default
    let! result = runRandomTests config arb property
    match result with
    | Some counterExample when config.MaxShrinks > 0 ->
        let! shrink = runShrinker config arb property config.MaxShrinks counterExample
        return Some shrink

    | _ -> return result
}

let extractPropertyTest (property : 'T -> 'S) =
    let lift prop t = async {
        let! result = async { return! prop t } |> Async.Catch
        return
            match result with
            | Choice1Of2 (Bool false) -> Falsified
            | Choice1Of2 _ -> Completed
            | Choice2Of2 e -> Exception e
    }

    match typeof<'S> with
    | Basic t when t = typeof<Task> ->
        let property = unbox<'T -> Task> property
        lift (fun t -> async { return! Async.AwaitTaskCorrect(property t) })

    | Generic(gt, [|arg|]) when gt = typedefof<Async<_>> ->
        TypeShape.Create(arg).Accept {
            new IFunc<AsyncProperty<'T>> with
                member __.Invoke<'R> () =
                    let property = unbox<'T -> Async<'R>> property
                    lift property
        }

    | Generic(gt, [|arg|]) when gt = typedefof<Task<_>> ->
        TypeShape.Create(arg).Accept {
            new IFunc<AsyncProperty<'T>> with
                member __.Invoke<'R> () =
                    let property = unbox<'T -> Task<'R>> property
                    lift (fun t -> async { return! Async.AwaitTaskCorrect(property t) })
        }

    | _ -> lift (fun t -> async { return property t })


let extractProperty (argument : obj option) (methodInfo : MethodInfo) =
    let argument = defaultArg argument null
    let inputTy, valueReader = 
        match methodInfo.GetParameters() |> Array.map (fun p -> p.ParameterType) with
        | [||] -> typeof<unit>, fun _ -> [|box ()|]
        | [|ty|] -> ty, fun v -> [|v|]
        | types -> 
            let tupleTy = FSharpType.MakeTupleType types
            tupleTy, FSharpValue.PreComputeTupleReader tupleTy

    let outputTy = 
        match methodInfo.ReturnType with 
        | rt when rt = typeof<Void> -> typeof<unit> 
        | rt -> rt

    let inputShape = TypeShape.Create inputTy
    let outputShape = TypeShape.Create outputTy

    inputShape.Accept {
      new IFunc<TypeShape * obj> with
        member __.Invoke<'T> () =
          outputShape.Accept {
            new IFunc<TypeShape * obj> with
              member __.Invoke<'R> () =
                let func =
                    (fun (t:'T) -> 
                        try methodInfo.Invoke(argument, valueReader t) :?> 'R
                        with :? TargetInvocationException as e ->
                            ExceptionDispatchInfo.Capture(e.InnerException).Throw()
                            failwithf "Should not get here - please report a bug")
                    |> extractPropertyTest

                inputShape, box func
            }
    }