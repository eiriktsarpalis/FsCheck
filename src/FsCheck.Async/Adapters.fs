[<AutoOpen>]
module internal FsCheck.Async.Adapters

open System
open System.Reflection
open System.Runtime.ExceptionServices
open System.Threading.Tasks

open FSharp.Reflection

open FsCheck.Async

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


let liftPropertyFromMethodInfo (argument : obj option) (methodInfo : MethodInfo) =
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
                            failwith "Should not get here - please report a bug")
                    |> liftPropertyTest

                inputShape, box func
            }
    }