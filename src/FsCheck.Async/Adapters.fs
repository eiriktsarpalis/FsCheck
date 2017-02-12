[<AutoOpen>]
module internal FsCheck.Async.Adapters

open System
open System.Reflection
open System.Runtime.ExceptionServices
open System.Threading.Tasks

open FSharp.Reflection

open FsCheck
open FsCheck.TypeClass
open FsCheck.Async

// TypeClass functionality replicated here since internal in FsCheck proper
// NB this creates slightly different semantics when invoking `Arb.from` in
// the current thread
let private defaultArbitrary = 
    let empty = FsCheck.TypeClass.TypeClass<Arbitrary<obj>>.New()
    empty.Discover(onlyPublic=true,instancesType=typeof<FsCheck.Arb.Default>)

let getArb<'TArb> (typeClasses : Type list) : Arbitrary<'TArb> =
    let merge t (tc:TypeClass<_>) = tc.DiscoverAndMerge(onlyPublic=true,instancesType=t)
    let tc = List.foldBack merge typeClasses defaultArbitrary
    tc.InstanceFor<'TArb, Arbitrary<'TArb>>()

/// Lifts a generic arrow instance into an asynchronous property test
let liftPropertyTest (property : 'T -> 'S) : AsyncProperty<'T> =
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


/// Lifts a method info instance into an existentially packed AsyncProperty
let liftPropertyFromMethodInfo (mkInstance : (unit -> obj) option) (methodInfo : MethodInfo) =
    let inputTy, argReader = 
        match methodInfo.GetParameters() |> Array.map (fun p -> p.ParameterType) with
        | [||] -> typeof<unit>, fun _ -> [||]
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

    // Use TypeShape to bring function shape into scope
    inputShape.Accept {
      new IFunc<TypeShape * obj> with
        member __.Invoke<'T> () =
          outputShape.Accept {
            new IFunc<TypeShape * obj> with
              member __.Invoke<'R> () =
                let func =
                    fun (t:'T) -> 
                        let instance = match mkInstance with None -> null | Some b -> b()
                        let args = argReader t
                        try methodInfo.Invoke(instance, args) :?> 'R
                        with :? TargetInvocationException as e ->
                            ExceptionDispatchInfo.Capture(e.InnerException).Throw()
                            failwith "Should not get here - please report a bug"
                    |> liftPropertyTest

                inputShape, box func
            }
    }