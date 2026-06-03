namespace DynamicObj


#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT || !FABLE_COMPILER

open Fable.Core

module FableJS =
    
    module PropertyDescriptor = 
        
        [<Emit("$0[$1]")>]
        let tryGetPropertyValue (o:obj) (propName:string) : obj option =
            jsNative

        let tryGetIsWritable (o:obj) : bool option =
            tryGetPropertyValue o "writable"
            |> Option.map (fun v -> v :?> bool)

        let containsGetter (o:obj) : bool =
            match tryGetPropertyValue o "get" with
            | Some _ -> true
            | None -> false

        let containsSetter (o:obj) : bool =
            match tryGetPropertyValue o "set" with
            | Some _ -> true
            | None -> false

        let isWritable (o:obj) : bool =
            match tryGetIsWritable o with
            | Some v -> v
            | None -> containsSetter o

        [<Emit("typeof $0 === 'function'")>]
        let valueIsFunction (o:obj) : bool =
            jsNative

        let isFunction (o:obj) : bool =
            match tryGetPropertyValue o "value" with
            | Some v -> valueIsFunction v
            | None -> false

    [<Emit("Object.getOwnPropertyNames($0)")>]
    let getOwnPropertyNames (o:obj) : string [] =
        jsNative

    [<Emit("Object.getPrototypeOf($0)")>]
    let getPrototype (o:obj) : obj =
        jsNative

    let getStaticPropertyNames (o:obj) =
        getPrototype o
        |> getOwnPropertyNames
        |> Array.filter (fun n -> n <> "constructor")

    [<Emit("$0.Properties")>]
    let getDynamicPropertyObjects (o: obj) : System.Collections.Generic.Dictionary<string,obj> =
        jsNative

    [<Emit("$0.Properties.has($1)")>]
    let hasStoredPropertyValue (o: obj) (propName: string) : bool =
        jsNative

    [<Emit("$0.Properties.get($1)")>]
    let getStoredPropertyValue (o: obj) (propName: string) : obj =
        jsNative

    [<Emit("$0.Properties.set($1, $2)")>]
    let setStoredPropertyValue (o: obj) (propName: string) (value: obj) : unit =
        jsNative

    // Track DynamicObj-owned runtime mirrors outside the object so user keys cannot collide.
    // WeakMap entries are collected with their objects and do not reserve any property name.
    [<Emit("""globalThis[Symbol.for("DynamicObj.mirrorSets")] || (globalThis[Symbol.for("DynamicObj.mirrorSets")] = new WeakMap())""")>]
    let getMirrorSets () : obj =
        jsNative

    // Check whether this object already has mirror ownership metadata.
    [<Emit("$0.has($1)")>]
    let mirrorStoreHas (store: obj) (o: obj) : bool =
        jsNative

    // Start tracking names that DynamicObj itself mirrored onto this object.
    [<Emit("$0.set($1, new Set())")>]
    let mirrorStoreCreate (store: obj) (o: obj) : unit =
        jsNative

    // Only names in this set may be removed from the runtime object later.
    [<Emit("$0.has($1) && $0.get($1).has($2)")>]
    let mirrorStoreHasProperty (store: obj) (o: obj) (propName: string) : bool =
        jsNative

    // Mark a runtime property as owned by DynamicObj's compatibility mirror.
    [<Emit("$0.get($1).add($2)")>]
    let mirrorStoreAddProperty (store: obj) (o: obj) (propName: string) : unit =
        jsNative

    // Unmark ownership after removing a DynamicObj-owned runtime mirror.
    [<Emit("$0.has($1) && $0.get($1).delete($2)")>]
    let mirrorStoreDeleteProperty (store: obj) (o: obj) (propName: string) : unit =
        jsNative

    let hasMirrorSet (o: obj) =
        mirrorStoreHas (getMirrorSets ()) o

    let createMirrorSet (o: obj) =
        mirrorStoreCreate (getMirrorSets ()) o

    let ensureMirrorSet (o: obj) =
        if not (hasMirrorSet o) then
            createMirrorSet o

    let isMirroredProperty (o: obj) (propName: string) =
        mirrorStoreHasProperty (getMirrorSets ()) o propName

    let markMirroredProperty (o: obj) (propName: string) =
        mirrorStoreAddProperty (getMirrorSets ()) o propName

    let unmarkMirroredProperty (o: obj) (propName: string) =
        mirrorStoreDeleteProperty (getMirrorSets ()) o propName

    [<Emit("$1 in $0")>]
    let hasRuntimeProperty (o: obj) (propName: string) : bool =
        jsNative

    [<Emit("$0[$1] = $2")>]
    let setRuntimeProperty (o: obj) (propName: string) (value: obj) : unit =
        jsNative

    [<Emit("delete $0[$1]")>]
    let deleteRuntimeProperty (o: obj) (propName: string) : unit =
        jsNative

    // Mirror only new names, or names already owned by our mirror; never overwrite typed/runtime members.
    let shouldMirrorDynamicProperty (o: obj) (propName: string) =
        isMirroredProperty o propName || not (hasRuntimeProperty o propName)

    // Keep native JS access like obj.extension working while Properties remains authoritative.
    let mirrorDynamicProperty (o: obj) (propName: string) (value: obj) =
        if shouldMirrorDynamicProperty o propName then
            ensureMirrorSet o
            setRuntimeProperty o propName value
            markMirroredProperty o propName

    // Remove only mirrors created by DynamicObj, leaving existing runtime members intact.
    let removeDynamicPropertyMirror (o: obj) (propName: string) =
        if isMirroredProperty o propName then
            deleteRuntimeProperty o propName
            unmarkMirroredProperty o propName

    // Dynamic SetProperty writes to Properties first, then updates the optional native mirror.
    let setPropertyValue (o:obj) (propName:string) (value:obj) =
        setStoredPropertyValue o propName value
        mirrorDynamicProperty o propName value

    let createSetter (propName:string) =
        fun (o:obj) (value:obj) -> 
         setPropertyValue o propName value

    [<Emit("$0[$1] = $2")>]
    let setStaticPropertyValue (o:obj) (propName:string) (value:obj) : unit =
        jsNative

    let createStaticSetter (propName:string) =
        fun (o:obj) (value:obj) ->
         setStaticPropertyValue o propName value

    let removeStaticPropertyValue (o:obj) (propName:string) =
        setStaticPropertyValue o propName null

    [<Emit("$0.Properties.delete($1)")>]
    let deleteStoredPropertyValue (o:obj) (propName:string) : unit =
        jsNative

    let deleteDynamicPropertyValue (o:obj) (propName:string) =
        deleteStoredPropertyValue o propName
        removeDynamicPropertyMirror o propName

    let createRemover (propName:string) (isStatic : bool) =
        if isStatic then
            fun (o:obj) -> 
                removeStaticPropertyValue o propName
        else
            fun (o:obj) -> 
                deleteDynamicPropertyValue o propName


    let getPropertyValue (o:obj) (propName:string) =
        getStoredPropertyValue o propName

    let createGetter (propName:string) =
        fun (o:obj) -> 
            getPropertyValue o propName

    [<Emit("$0[$1]")>]
    let getStaticPropertyValue (o:obj) (propName:string) : obj =
        jsNative

    let createStaticGetter (propName:string) =
        fun (o:obj) ->
            getStaticPropertyValue o propName

    [<Emit("Object.getOwnPropertyDescriptor($0, $1)")>]
    let tryGetPropertyDescriptor (o:obj) (propName:string) : obj option =
        jsNative

    let tryGetStaticPropertyDescriptor (o:obj) (propName:string) : obj option=
        tryGetPropertyDescriptor (getPrototype o) propName

    let tryStaticPropertyHelperFromDescriptor (pd:obj) (name:string) : PropertyHelper option =
        let isWritable = PropertyDescriptor.isWritable pd
        if PropertyDescriptor.isFunction pd then 
            None
        else 
            {
                Name = name
                IsStatic = true
                IsDynamic = false
                IsMutable = isWritable
                IsImmutable = not isWritable
                GetValue = createStaticGetter name
                SetValue = createStaticSetter name
                RemoveValue = createRemover name true
            }     
            |> Some
        
    let tryGetStaticPropertyHelper (o:obj) (propName:string) : PropertyHelper option =
        tryGetStaticPropertyDescriptor o propName
        |> Option.bind (fun pd -> tryStaticPropertyHelperFromDescriptor pd propName)
        
    let getStaticPropertyHelpers (o:obj) : PropertyHelper [] =
        getStaticPropertyNames o
        |> Array.choose (tryGetStaticPropertyHelper o)

    let tryGetDynamicPropertyHelper (o:obj) (propName:string) : PropertyHelper option =
        if hasStoredPropertyValue o propName then
            {
                Name = propName
                IsStatic = false
                IsDynamic = true
                IsMutable = true
                IsImmutable = false
                GetValue = createGetter propName
                SetValue = createSetter propName
                RemoveValue = createRemover propName false
            }
            |> Some
        else
            None

    let getDynamicPropertyHelpers (o:obj) : PropertyHelper [] =
        getDynamicPropertyObjects o
        |> Seq.map (fun kv ->
            {
                Name = kv.Key
                IsStatic = false
                IsDynamic = true
                IsMutable = true
                IsImmutable = false
                GetValue = createGetter kv.Key
                SetValue = createSetter kv.Key
                RemoveValue = createRemover kv.Key false
            }
        )
        |> Seq.toArray

    // Used by ofDict because assigning Properties directly bypasses SetProperty mirroring.
    let syncRuntimeDynamicProperties (o: obj) =
        getDynamicPropertyObjects o
        |> Seq.iter (fun kv -> mirrorDynamicProperty o kv.Key kv.Value)

    let getPropertyHelpers (o:obj) =
        getDynamicPropertyHelpers o
        |> Array.append (getStaticPropertyHelpers o)

    let getPropertyNames (o:obj) =
        getPropertyHelpers o 
        |> Array.map (fun h -> h.Name)

    module Interfaces = 
        
        [<Emit("""$0["System.ICloneable.Clone"] != undefined && (typeof $0["System.ICloneable.Clone"]) === 'function'""")>]
        let implementsICloneable (o:obj) : bool =
            jsNative

        [<Emit("""$0["System.ICloneable.Clone"]()""")>]
        let cloneICloneable (o:obj) : obj =
            jsNative

    module Dictionaries =
        [<Emit("""$0 instanceof Map""")>]
        let isMap (o:obj) : bool =
            jsNative
        [<Emit("""$0 instanceof Dictionary""")>]
        let isDict (o:obj) : bool =
            jsNative
#endif
