namespace FsCheck.Async

open System.Threading.Tasks

open FsCheck

type AsyncCheck =

    static member One(property : 'T -> Async<'S>, ?config : AsyncConfig, ?arb : Arbitrary<'T>) = async {
        let property' t = async {
            let! result = property t |> Async.Catch
            return
                match result with
                | Choice1Of2 (Bool false) -> Falsified
                | Choice1Of2 _ -> Completed
                | Choice2Of2 e -> Exception e
        }

        return! runPropertyTestAsync config arb property'
    }

    static member One(property : 'T -> Task<'S>, ?config : AsyncConfig, ?arb : Arbitrary<'T>) = async {
        return! AsyncCheck.One((fun t -> async { return! Async.AwaitTaskCorrect(property t) }), 
                                    ?config = config, ?arb = arb)
    }

    static member One(property : 'T -> Task, ?config : AsyncConfig, ?arb : Arbitrary<'T>) = async {
        return! AsyncCheck.One((fun t -> async { return! Async.AwaitTaskCorrect(property t) }), 
                                    ?config = config, ?arb = arb)
    }