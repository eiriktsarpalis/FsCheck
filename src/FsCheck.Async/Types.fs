namespace FsCheck.Async

open FsCheck.Random

type AsyncConfig =
    {
        MaxTest : int
        MaxShrinks : int
        StartSize : int
        EndSize : int
        Replay : StdGen option
        DegreeOfParallelism : int
    }
with
    static member Default =
        {
            MaxTest = FsCheck.Config.Default.MaxTest
            MaxShrinks = 20
            StartSize = FsCheck.Config.Default.StartSize
            EndSize = FsCheck.Config.Default.EndSize
            Replay = None
            DegreeOfParallelism = System.Environment.ProcessorCount * 2
        }

type AsyncProperty<'T> = 'T -> Async<TestOutcome>

and TestOutcome =
    | Completed
    | Falsified
    | Exception of exn

and CounterExample<'T> = 
    { 
        StdGen : StdGen 
        Size : int 
        Value : 'T
        Outcome : TestOutcome
        Shrinks : ('T * TestOutcome) list 
    }