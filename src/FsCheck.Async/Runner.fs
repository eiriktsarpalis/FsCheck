[<AutoOpen>]
module internal FsCheck.Async.Runner

open System
open System.Threading
open System.Threading.Tasks

open FsCheck
open FsCheck.Random

type CounterExample<'T> with
    member __.SmallestValue =
        match __.Shrinks with
        | (v,_) :: _ -> v
        | [] -> failwithf "internal error"

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