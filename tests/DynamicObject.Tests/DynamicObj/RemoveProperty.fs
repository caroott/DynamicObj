module RemoveProperty

open Fable.Pyxpecto
open DynamicObj
open TestUtils

let tests_RemoveProperty = testList "RemoveProperty" [
  
    //TODO: static property removal!

    testCase "Remove" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        a.SetProperty("quack!", "hello")

        a.RemoveProperty "quack!" |> ignore

        Expect.equal a b "Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be equal"

    testCase "Remove Non-Existing" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        a.SetProperty("quack!", "hello")
        b.SetProperty("quack!", "hello")

        a.RemoveProperty "quecky!" |> ignore

        Expect.equal a b "Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be equal"

    testCase "Remove only on one" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        a.SetProperty("quack!", "hello")
        b.SetProperty("quack!", "hello")

        a.RemoveProperty "quack!" |> ignore

        Expect.notEqual a b "Values should be unequal"
        Expect.notEqual (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be unequal"

    testCase "Nested Remove Non-Existing" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3])

        a.SetProperty("aaa", a')
        a.RemoveProperty "quack!" |> ignore
        b.SetProperty("aaa", b')

        Expect.equal a b "Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be equal"

    testCase "Nested Remove only on one" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3])

        a.SetProperty("aaa", a')
        a'.RemoveProperty "quack!" |> ignore
        b.SetProperty("aaa", b')

        Expect.notEqual a b "Values should be unequal"
        Expect.notEqual (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be unequal"

    testCase "Nested Remove on both" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3])

        a.SetProperty("aaa", a')
        a.RemoveProperty "quack!" |> ignore
        b.SetProperty("aaa", b')
        b.RemoveProperty "quack!" |> ignore

        Expect.equal a b "Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be equal"

    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT || FABLE_COMPILER_PYTHON
    testCase "RemoveProperty removes safe native runtime property mirrors" <| fun _ ->
        let a = DynamicObj()
        a.SetProperty("extension", box 42)
        Expect.isTrue (NativeRuntime.hasProperty a "extension") "Mirror should exist before removal"
        Expect.isTrue (a.RemoveProperty("extension")) "RemoveProperty should remove the dynamic property"
        Expect.isFalse (NativeRuntime.hasProperty a "extension") "Mirror should be removed with the dynamic property"
        Expect.isNone (a.TryGetDynamicPropertyHelper("extension")) "Dynamic helper should no longer exist"
    #endif

]
