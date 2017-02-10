namespace FsCheck.Async.Xunit

open System
open System.Threading.Tasks

open Xunit
open Xunit.Sdk
open Xunit.Abstractions

open FsCheck.Async

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("FsCheck.Async.Xunit.AsyncPropertyDiscoverer", "FsCheck.Async")>]
type public AsyncPropertyAttribute() =
    inherit FactAttribute()
    let mutable config = AsyncConfig.Default

    ///The maximum number of tests that are run.
    member __.MaxTest with get() = config.MaxTest and set(v) = config <- {config with MaxTest = v }
    ///The maximum number of shrinks to run after a counterexample was encountered
    member __.MaxShrinks with get() = config.MaxShrinks and set(v) = config <- {config with MaxShrinks = v}
    ///The size to use for the first test.
    member __.StartSize with get() = config.StartSize and set(v) = config <- {config with StartSize = v}
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = config.EndSize and set(v) = config <- {config with EndSize = v}

    member internal __.Config = config


type AsyncPropertyTestCase(diagnosticMessageSink:IMessageSink, defaultMethodDisplay:TestMethodDisplay, testMethod:ITestMethod, ?testMethodArguments:obj []) =
    inherit XunitTestCase(diagnosticMessageSink, defaultMethodDisplay, testMethod, (match testMethodArguments with | None -> null | Some v -> v))

    new() = new AsyncPropertyTestCase(null, TestMethodDisplay.ClassAndMethod, null)

    override this.RunAsync(_diagnosticMessageSink:IMessageSink, messageBus:IMessageBus, constructorArguments:obj [], _aggregator:ExceptionAggregator, cancellationTokenSource:Threading.CancellationTokenSource) =
        let test = new XunitTest(this, this.DisplayName)
        let summary = new RunSummary(Total = 1)
        let outputHelper = new TestOutputHelper()
        outputHelper.Initialize(messageBus, test)
        let testAsync() = async {
            let factAttribute = this.TestMethod.Method.GetCustomAttributes(typeof<AsyncPropertyAttribute>) |> Seq.head
            let config = factAttribute.GetNamedArgument<AsyncConfig> "Config"
            let timer = ExecutionTimer()
            let! result = async {
                try
                    let runMethod = this.TestMethod.Method.ToRuntimeMethod()
                    let target =
                        constructorArguments
                        |> Array.tryFind (fun x -> x :? TestOutputHelper)
                        |> Option.iter (fun x -> (x :?> TestOutputHelper).Initialize(messageBus, test))

                        let testClass = this.TestMethod.TestClass.Class.ToRuntimeType()
                        if this.TestMethod.TestClass <> null && not this.TestMethod.Method.IsStatic then
                            Some (test.CreateTestClass(testClass, constructorArguments, messageBus, timer, cancellationTokenSource))
                        else None

                    let counterExample = ref None
                    do!
                        fun () ->
                            async { 
                                let! res = AsyncCheck.Method(runMethod, config, ?target = target) 
                                counterExample := res }
                            |> Async.StartAsTask 
                            :> Task
                        |> timer.AggregateAsync
                        |> Async.AwaitTaskCorrect

                    return
                        match !counterExample with
                        | Some { StdGen = stdGen; Size = size; Shrinks = (value, Falsified) :: _ } ->
                            let message = sprintf "Test falsified: Value=%s, StdGen=%A, Size=%d" value stdGen size
                            summary.Failed <- summary.Failed + 1
                            TestFailed(test, timer.Total, message, exn message) :> TestResultMessage
                        | Some { StdGen = stdGen; Size = size; Shrinks = (value, Exception e) :: _ } ->
                            let message = 
                                sprintf "Test exception: %O%s Value=%s, StdGen=%A, Size=%d"
                                                e Environment.NewLine value stdGen size

                            summary.Failed <- summary.Failed + 1
                            TestFailed(test, timer.Total, message, exn message) :> _

                        | _ -> TestPassed(test, timer.Total, "Test passed.") :> _

                with e ->
                    return TestFailed(test, timer.Total, "Exception during test", e) :> _
            }

            messageBus.QueueMessage(result) |> ignore
            summary.Time <- summary.Time + result.ExecutionTime
            if not (messageBus.QueueMessage(new TestFinished(test, summary.Time, result.Output))) then
                cancellationTokenSource.Cancel() |> ignore
            return summary
        }


        if not (messageBus.QueueMessage(new TestStarting(test))) then
            cancellationTokenSource.Cancel() |> ignore

        if not(String.IsNullOrEmpty(this.SkipReason)) then
            summary.Skipped <- summary.Skipped + 1
            if not(messageBus.QueueMessage(new TestSkipped(test, this.SkipReason))) then
                cancellationTokenSource.Cancel() |> ignore
            Task.Factory.StartNew(fun () -> summary)
        else
            Async.StartAsTask(testAsync())


/// xUnit2 test case discoverer to link the method with the PropertyAttribute to the PropertyTestCase
/// so the test can be run via FsCheck.
type PropertyDiscoverer(messageSink:IMessageSink) =

    new () = PropertyDiscoverer(null)

    member __.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, _:IAttributeInfo)=
            let ptc = new AsyncPropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod)
            Seq.singleton (ptc :> IXunitTestCase)