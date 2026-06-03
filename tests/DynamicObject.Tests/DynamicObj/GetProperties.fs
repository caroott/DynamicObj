module GetProperties

open Fable.Pyxpecto
open DynamicObj
open TestUtils

let tests_GetProperties = testList "GetProperties" [
    testCase "GetProperties" <| fun _ ->
        let a = DynamicObj()
        a.SetProperty("a", 1)
        a.SetProperty("b", 2)
        let properties = a.GetProperties(true) |> List.ofSeq
        let expected = [
            System.Collections.Generic.KeyValuePair("a", box 1)
            System.Collections.Generic.KeyValuePair("b", box 2)
        ]
        Expect.sequenceEqual properties expected "Should have all properties"
    testCase "returns static instance members of derived class when wanted" <| fun _ ->
        let a = DerivedClass(stat = "stat", dyn = "dyn")
        let properties = a.GetProperties(true) |> List.ofSeq |> List.sortBy (fun kv -> kv.Key)
        let expected = 
            [
                System.Collections.Generic.KeyValuePair("dyn", box "dyn")
                System.Collections.Generic.KeyValuePair("stat", box "stat")
            ] 
            |> Seq.sortBy (fun kv -> kv.Key)
        Expect.sequenceEqual properties expected "Should have all properties"

    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT || FABLE_COMPILER_PYTHON || !FABLE_COMPILER
    testCase "dynamic properties do not include mutable instance backing fields" <| fun _ ->
        let a = DerivedClassWithMutableInstanceProperty(location = "somewhere")
        a.SetProperty("extension", 42)
        let properties = a.GetProperties(false) |> List.ofSeq
        let expected = [
            System.Collections.Generic.KeyValuePair("extension", box 42)
        ]
        Expect.sequenceEqual properties expected "Should only return dynamic extension properties"
    #endif

    testCase "dynamic properties may use names that look generated" <| fun _ ->
        let a = DynamicObj()
        a.SetProperty("stat@13", box "js-like")
        a.SetProperty("_location", box "py-like")
        let properties =
            a.GetProperties(false)
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.sortBy fst
            |> List.ofSeq
        let expected = [
            "_location", box "py-like"
            "stat@13", box "js-like"
        ]
        Expect.sequenceEqual properties expected "Dynamic property names must not be filtered because they look generated"

    testCase "GetProperties true includes declared properties and dynamic properties without backing fields" <| fun _ ->
        let a = DerivedClassWithMutableInstanceProperty(location = "somewhere")
        a.SetProperty("extension", box 42)
        let properties =
            a.GetProperties(true)
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.sortBy fst
            |> List.ofSeq
        let expected = [
            "Location", box (Some "somewhere")
            "extension", box 42
        ]
        Expect.sequenceEqual properties expected "Should include declared property and dynamic property only"
]
