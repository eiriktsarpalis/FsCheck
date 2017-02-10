[<AutoOpen>]
module internal FsCheck.Async.Runner

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
          Shrinks = __.Shrinks |> List.map (fun (t,s) -> box t, s) }

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
    let! ct = Async.CancellationToken
    use runner = new ThrottledNonDeterministicRunner<CounterExample<'T>>(config.DegreeOfParallelism, ct)
    use enum = randomItems.GetEnumerator()
    while enum.MoveNext() && not runner.IsCompleted do
        // NB: we are intentionally only parallelizing the property run
        //     but not the value generator to avoid exposing races
        let size, stdGen = enum.Current
        let value = Gen.eval size stdGen arb.Generator

        let instanceRunner = async {
            let! outcome = async {
                try return! property value
                with e -> return Exception e
            }

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
                    (numShrinks : int) (shrink : CounterExample<'T>) = async {

    if numShrinks = config.MaxShrinks then return shrink else

    let shrinkCandidates = arb.Shrinker shrink.SmallestValue

    let! ct = Async.CancellationToken
    use runner = new ThrottledNonDeterministicRunner<CounterExample<'T>>(config.DegreeOfParallelism, ct)
    use enum = shrinkCandidates.GetEnumerator()
    while enum.MoveNext() && not runner.IsCompleted do
        // NB: we are intentionally only parallelizing the property run
        //     but not the shrinker to avoid exposing races
        let shrinkValue = enum.Current
        let instanceRunner = async {
            let! outcome = async { 
                try return! property shrinkValue 
                with e -> return Exception e
            }

            match outcome with
            | Completed -> return None
            | Falsified
            | Exception _ -> return Some { shrink with Shrinks = (shrinkValue, outcome) :: shrink.Shrinks }
        }

        do! runner.Enqueue instanceRunner

    let! shrinkSearchResult = runner.AwaitCompletion()

    match shrinkSearchResult with
    | None -> return shrink
    | Some result -> return! runShrinker config arb property numShrinks result
}

let runPropertyTestAsync (config : AsyncConfig option) (arb : Arbitrary<'T> option) (property : AsyncProperty<'T>) = async {
    let arb = match arb with Some a -> a | None -> Arb.from<'T>
    let config = match config with Some c -> c | None -> AsyncConfig.Default
    let! result = runRandomTests config arb property
    match result with
    | Some counterExample when config.MaxShrinks > 0 ->
        let! shrink = runShrinker config arb property 0 counterExample
        return Some shrink

    | _ -> return result
}