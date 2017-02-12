[<AutoOpen>]
module internal FsCheck.Async.Runner

open FsCheck
open FsCheck.Random
open FsCheck.Async

type CounterExample<'T> with
    member __.SmallestValue =
        match __.Shrinks with
        | (v,_) :: _ -> v
        | [] -> __.Value

    member __.ToUntyped =
        { StdGen = __.StdGen ; Size = __.Size ; Value = box __.Value ; Outcome = __.Outcome ;
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

    let seeds = 
        genSeeds initSize seed
        |> Seq.take config.MaxTest

    // the runner segment to be run in async/parallel fashion
    let runTest (size, stdGen) = 
        // NB value generation is intentionally lifted outside of the async block
        //    this is to ensure consumption is made in sequential fashion and
        //    avoid potential race conditions in generator implementations
        let value = Gen.eval size stdGen arb.Generator
        async {
            let! outcome = property value
            match outcome with
            | Completed -> return None
            | Falsified
            | Exception _ -> 
                return Some { 
                    StdGen = stdGen ; Size = size ; 
                    Value = value; Outcome = outcome ; Shrinks = [] }
        }

    return! 
        seeds
        |> Seq.map runTest 
        |> Async.ChoiceThrottled config.DegreeOfParallelism
}

let rec runShrinker (config : AsyncConfig)
                    (arb : Arbitrary<'T>) (property : AsyncProperty<'T>)
                    (numShrinks : int) (state : CounterExample<'T>) = async {

    if numShrinks = config.MaxShrinks then return state else

    let shrinkCandidates = arb.Shrinker state.SmallestValue

    let runTest shrinkCandidate = async {
        let! outcome = property shrinkCandidate
        match outcome with
        | Completed -> return None
        | Falsified
        | Exception _ -> return Some { state with Shrinks = (shrinkCandidate, outcome) :: state.Shrinks }
    }

    let! shrinkResult = 
        shrinkCandidates 
        |> Seq.map runTest 
        |> Async.ChoiceThrottled config.DegreeOfParallelism

    match shrinkResult with
    | None -> return state
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