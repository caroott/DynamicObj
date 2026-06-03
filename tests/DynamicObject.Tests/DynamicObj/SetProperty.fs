module SetProperty

open Fable.Pyxpecto
open DynamicObj
open TestUtils

let tests_SetProperty = testList "SetProperty" [

    //TODO: static property accession!

    testCase "Same String" <| fun _ ->
        let a = DynamicObj ()
        a.SetProperty("aaa", 5)
        let b = DynamicObj ()
        b.SetProperty("aaa", 5)
        Expect.equal a b "Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Hash codes should be equal"

    testCase "Different Strings" <| fun _ ->
        let a = DynamicObj ()
        a.SetProperty("aaa", 1212)
        let b = DynamicObj ()
        b.SetProperty("aaa", 5)
        Expect.notEqual a b "Values should not be equal"

    testCase "String only on one" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()
        b.SetProperty("aaa", 5)

        Expect.notEqual a b "Values should not be equal"
        Expect.notEqual b a "Values should not be equal (Reversed equality)"

    testCase "Same lists different keys" <| fun _ ->
        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!1", [1; 2; 3])
        Expect.notEqual (a'.GetHashCode()) (b'.GetHashCode()) "Hash codes should not be equal"   
   
    testCase "Different lists" <| fun _ ->
        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3; 4; 34])
        Expect.notEqual (a'.GetHashCode()) (b'.GetHashCode()) "Hash codes should not be equal"

    testCase "Nested Same List Same String" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3])

        a.SetProperty("aaa", a')
        b.SetProperty("aaa", b')
        Expect.equal a' b' "New Values should be equal"
        Expect.equal a b "Old Values should be equal"
        Expect.equal (a.GetHashCode()) (b.GetHashCode()) "Old Hash codes should be equal"
        Expect.equal (a'.GetHashCode()) (b'.GetHashCode()) "New Hash codes should be equal"

    testCase "Nested Same List Different Strings" <| fun _ ->
        let a = DynamicObj ()
        let b = DynamicObj ()

        let a' = DynamicObj ()
        let b' = DynamicObj ()
        a'.SetProperty("quack!", [1; 2; 3])
        b'.SetProperty("quack!", [1; 2; 3])

        a.SetProperty("aaa", a')
        b.SetProperty("aaa1", b')
        Expect.equal a' b' "New Values should be equal"
        Expect.notEqual a b "Old Values should not be equal"
        Expect.equal (a'.GetHashCode()) (b'.GetHashCode()) "New Hash codes should be equal"

    #if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT || FABLE_COMPILER_PYTHON
    testCase "SetProperty exposes safe dynamic properties as native runtime properties" <| fun _ ->
        let a = DynamicObj()
        a.SetProperty("extension", box 42)
        Expect.isTrue (NativeRuntime.hasProperty a "extension") "Safe dynamic property should exist on the runtime object"
        Expect.equal (NativeRuntime.getProperty a "extension") (box 42) "Runtime property should match SetProperty value"
        a.SetProperty("extension", box 43)
        Expect.equal (NativeRuntime.getProperty a "extension") (box 43) "Runtime property should update with SetProperty"
        Expect.equal (a.GetPropertyValue("extension")) (box 43) "DynamicObj API should use the same value"

    testCase "SetProperty exposes mirror-internal property names as native runtime properties" <| fun _ ->
        let a = DynamicObj()
        a.SetProperty("_dynamicObjMirroredProperties", box 42)
        Expect.equal (a.GetPropertyValue("_dynamicObjMirroredProperties")) (box 42) "DynamicObj API should read mirror-internal dynamic names"
        Expect.isTrue (NativeRuntime.hasProperty a "_dynamicObjMirroredProperties") "Mirror-internal dynamic name should exist on the runtime object"
        Expect.equal (NativeRuntime.getProperty a "_dynamicObjMirroredProperties") (box 42) "Runtime property should match SetProperty value"

    testCase "SetProperty does not overwrite existing backing fields when dynamic name collides" <| fun _ ->
        let a = DerivedClassWithMutableStringInstanceProperty("typed")
        a.SetProperty("_location", box "dynamic")
        Expect.equal (a.GetPropertyValue("_location")) (box "dynamic") "Dynamic value should be available through DynamicObj API"
        Expect.equal a.Location "typed" "Typed property should still read its backing field"

    testCase "ofDict exposes safe dynamic properties as native runtime properties" <| fun _ ->
        let dict = System.Collections.Generic.Dictionary<string,obj>()
        dict.Add("extension", box 42)
        let a = DynamicObj.ofDict dict
        Expect.equal (a.GetPropertyValue("extension")) (box 42) "DynamicObj API should read ofDict value"
        Expect.isTrue (NativeRuntime.hasProperty a "extension") "Safe ofDict dynamic property should be mirrored"
        Expect.equal (NativeRuntime.getProperty a "extension") (box 42) "Runtime mirror should match ofDict value"

    testCase "ofDict exposes mirror-internal property names as native runtime properties" <| fun _ ->
        let dict = System.Collections.Generic.Dictionary<string,obj>()
        dict.Add("_dynamicObjMirroredProperties", box 42)
        let a = DynamicObj.ofDict dict
        Expect.equal (a.GetPropertyValue("_dynamicObjMirroredProperties")) (box 42) "DynamicObj API should read ofDict mirror-internal value"
        Expect.isTrue (NativeRuntime.hasProperty a "_dynamicObjMirroredProperties") "Mirror-internal ofDict dynamic property should be mirrored"
        Expect.equal (NativeRuntime.getProperty a "_dynamicObjMirroredProperties") (box 42) "Runtime mirror should match ofDict value"

    testCase "SetProperty still routes declared mutable properties through their setter" <| fun _ ->
        let a = DerivedClassWithMutableStringInstanceProperty("before")
        a.SetProperty("Location", box "after")
        Expect.equal a.Location "after" "Declared property setter should be used"
        Expect.isNone (a.TryGetDynamicPropertyHelper("Location")) "Declared property should not be stored as dynamic"
    #endif
    ]
