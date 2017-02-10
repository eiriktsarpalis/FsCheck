namespace FsCheck.Async

open System
open System.Reflection
open System.Threading.Tasks

open FSharp.Reflection

open FsCheck
open FsCheck.Async

type AsyncCheck =

    static member One(property : 'T -> 'S, ?config : AsyncConfig, ?arb : Arbitrary<'T>) = async {
        let func = liftPropertyTest property
        return! runPropertyTestAsync config arb func
    }

    static member Method(methodInfo:MethodInfo, ?config : AsyncConfig, ?target : obj) = async {
        let shape, func = liftPropertyFromMethodInfo target methodInfo
        return! shape.Accept { new IFunc<Async<CounterExample<obj> option>> with
            member __.Invoke<'T>() = async {
                let! result = runPropertyTestAsync config None (func :?> AsyncProperty<'T>)
                return result |> Option.map (fun r -> r.ToUntyped)
            }
        }
    }