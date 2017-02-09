#I "bin/Debug"
#r "FsCheck.dll"
#r "FsCheck.Async.dll"

open FsCheck
open FsCheck.Async

AsyncCheck.One((fun (ts : int[]) -> async { return Array.length ts < 10 }))
|> Async.RunSynchronously