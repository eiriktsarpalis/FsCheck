#I "bin/Debug"
#r "FsCheck.dll"
#r "FsCheck.Async.dll"

open FsCheck
open FsCheck.Async

AsyncCheck.One(fun (ts : int[]) -> Array.length ts < 2)
|> Async.RunSynchronously