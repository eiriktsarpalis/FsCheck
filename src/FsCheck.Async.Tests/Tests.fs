namespace FsCheck.Async.Tests

open Swensen.Unquote.Assertions
open Xunit
open FsCheck.Xunit
open FsCheck.Async.Xunit

module Tests =

    let predicate x = 
        System.Threading.Thread.Sleep 500
        x < 0 || x = 0 || x > 0

    [<Fact>]
    let ``Trisection Principle Unit`` () =
        test <@ predicate 42 @>

    [<Property>]
    let ``Trisection Principle`` (x : int) =
        predicate x

    [<AsyncProperty>]
    let ``Trisection Principle 2`` (x : int) =
        async { return predicate x }