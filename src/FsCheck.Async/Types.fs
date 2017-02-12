namespace FsCheck.Async

open System

open FsCheck
open FsCheck.Random

//// Just like config, but with the addition of MaxShrink and DegreeOfParallelism arguments
//
type AsyncConfig =
    {
        FsCheckConfig : FsCheck.Config
        MaxShrink : int option
        DegreeOfParallelism : int
    }
with
    static member FromFsCheckConfig(cfg : Config) =
        {
            FsCheckConfig = cfg
            MaxShrink = None
            DegreeOfParallelism = System.Environment.ProcessorCount * 2
        }

    static member Default = AsyncConfig.FromFsCheckConfig Config.Default

type AsyncProperty<'T> = 'T -> Async<TestOutcome>

and CounterExample<'T> = 
    { 
        StdGen : StdGen 
        Size : int 
        Value : 'T
        Outcome : TestOutcome
        Shrinks : ('T * TestOutcome) list 
    }

and TestOutcome =
    | Completed
    | Falsified
    | Exception of exn