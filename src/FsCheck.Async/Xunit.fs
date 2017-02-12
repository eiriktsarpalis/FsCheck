namespace FsCheck.Async.Xunit

open System
open System.Threading.Tasks

open Xunit
open Xunit.Sdk
open Xunit.Abstractions

open FsCheck.Random
open FsCheck.Async

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
[<XunitTestCaseDiscoverer("FsCheck.Async.Xunit.AsyncPropertyDiscoverer", "FsCheck.Async")>]
type public AsyncPropertyAttribute() =
    inherit FactAttribute()
    let mutable config = AsyncConfig.Default

    static let parseStdGen (str: string) =
        //if someone sets this, we want it to throw if it fails
        let split = str.Trim('(',')').Split([|","|], StringSplitOptions.RemoveEmptyEntries)
        let elem1 = Int32.Parse(split.[0])
        let elem2 = Int32.Parse(split.[1])
        StdGen (elem1,elem2)

    static let unparseStdGen (StdGen(s1,s2)) = sprintf "(%d,%d)" s1 s2

    let getCfg() = config.FsCheckConfig
    let updateCfg f = config <- { config with FsCheckConfig = f config.FsCheckConfig }

    ///The maximum number of shrinks to run after a counterexample was encountered
    member __.MaxShrink with get() = config.MaxShrink and set(v) = config <- {config with MaxShrink = v}
    ///The maximum number of random tests to run in parallel
    member __.DegreeOfParallelism with get() = config.DegreeOfParallelism and set(d) = config <- { config with DegreeOfParallelism = d }

    ///If set, the seed to use to start testing. Allows reproduction of previous runs. You can just paste
    ///the tuple from the output window, e.g. 12344,12312 or (123,123).
    member __.Replay 
        with get() = 
            match getCfg().Replay with
            | None -> null
            | Some g -> unparseStdGen g

        and set(v) = updateCfg (fun c -> { c with Replay = Some (parseStdGen v) })
    ///The maximum number of tests that are run.
    member __.MaxTest with get() = getCfg().MaxTest and set(v) = updateCfg (fun c -> {c with MaxTest = v })
    ///The size to use for the first test.
    member __.StartSize with get() = getCfg().StartSize and set(v) = updateCfg (fun c -> {c with StartSize = v})
    ///The size to use for the last test, when all the tests are passing. The size increases linearly between Start- and EndSize.
    member __.EndSize with get() = getCfg().EndSize and set(v) = updateCfg (fun c -> {c with EndSize = v})
    ///The Arbitrary instances to use for this test method. The Arbitrary instances
    ///are merged in back to front order i.e. instances for the same generated type
    ///at the front of the array will override those at the back.
    member __.Arbitrary with get() = List.toArray (getCfg().Arbitrary) and set(v) = updateCfg (fun c -> {c with Arbitrary = Array.toList v})
    ///If set, suppresses the output from the test if the test is successful. This can be useful when running tests
    ///with TestDriven.net, because TestDriven.net pops up the Output window in Visual Studio if a test fails; thus,
    ///when conditioned to that behaviour, it's always a bit jarring to receive output from passing tests.
    ///The default is false, which means that FsCheck will also output test results on success, but if set to true,
    ///FsCheck will suppress output in the case of a passing test. This setting doesn't affect the behaviour in case of
    ///test failures.
    member __.QuietOnSuccess with get() = getCfg().QuietOnSuccess and set(v) = updateCfg (fun c -> {c with QuietOnSuccess = v})

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
                    let instanceBuilder =
                        constructorArguments
                        |> Array.tryFind (fun x -> x :? TestOutputHelper)
                        |> Option.iter (fun x -> (x :?> TestOutputHelper).Initialize(messageBus, test))

                        let testClass = this.TestMethod.TestClass.Class.ToRuntimeType()
                        if this.TestMethod.TestClass <> null && not this.TestMethod.Method.IsStatic then
                            let instance = test.CreateTestClass(testClass, constructorArguments, messageBus, timer, cancellationTokenSource)
                            Some(fun () -> instance)
                        else None

                    let counterExample = ref None
                    do!
                        fun () ->
                            async { 
                                let! res = AsyncCheck.Method(runMethod, config, ?mkInstance = instanceBuilder) 
                                counterExample := res }
                            |> Async.StartAsTask 
                            :> Task
                        |> timer.AggregateAsync
                        |> Async.AwaitTaskCorrect

                    return
                        match !counterExample with
                        | Some ({ Outcome = Falsified } as counter) ->
                            let message = 
                                sprintf "Test falsified: Value=%A, StdGen=%A, Size=%d, Shrinks=%d" 
                                        counter.SmallestValue counter.StdGen counter.Size counter.Shrinks.Length

                            summary.Failed <- summary.Failed + 1
                            TestFailed(test, timer.Total, message, exn message) :> TestResultMessage
                        | Some ({ Outcome = Exception e } as counter) ->
                            let message = 
                                sprintf "Test exception: %O%s Value=%A, StdGen=%A, Size=%d, Shrinks=%d"
                                    e Environment.NewLine counter.SmallestValue counter.StdGen counter.Size counter.Shrinks.Length

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
            Async.StartAsTask(testAsync(), cancellationToken = cancellationTokenSource.Token)


/// xUnit2 test case discoverer to link the method with the PropertyAttribute to the PropertyTestCase
/// so the test can be run via FsCheck.
type AsyncPropertyDiscoverer(messageSink:IMessageSink) =

    new () = AsyncPropertyDiscoverer(null)

    member __.MessageSink = messageSink

    interface IXunitTestCaseDiscoverer with
        override this.Discover(discoveryOptions:ITestFrameworkDiscoveryOptions, testMethod:ITestMethod, _:IAttributeInfo)=
            let ptc = new AsyncPropertyTestCase(this.MessageSink, discoveryOptions.MethodDisplayOrDefault(), testMethod)
            Seq.singleton (ptc :> IXunitTestCase)