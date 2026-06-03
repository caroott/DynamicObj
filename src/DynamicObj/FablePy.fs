namespace DynamicObj


#if FABLE_COMPILER_PYTHON || !FABLE_COMPILER

open Fable.Core
open System.Collections.Generic

module FablePy =

    [<Emit("int($0)")>]
    let toPythonInt (value:int) : int =
        nativeOnly
    
    module Dictionary = 
        
        let ofSeq (s:seq<KeyValuePair<_,_>>) =
            let d = new System.Collections.Generic.Dictionary<_,_>()
            s |> Seq.iter (fun kv -> d.Add(kv.Key, kv.Value))
            d

        let choose (f: 'T -> 'U option) (d:System.Collections.Generic.Dictionary<_,'T>) =
            let nd = new System.Collections.Generic.Dictionary<_,'U>()
            for kv in d do
                match f kv.Value with
                | Some v -> nd.Add(kv.Key, v)
                | None -> ()
            nd

    type PropertyObject = 
        abstract fget : obj
        abstract fset : obj

    module PropertyObject = 
        
        [<Emit("$0.fget")>]
        let tryGetGetter (o:PropertyObject) : (obj -> obj) option =
            nativeOnly

        [<Emit("$0.fset")>]
        let tryGetSetter (o:PropertyObject) : (obj -> obj -> unit) option =
            nativeOnly

        let getGetter (o : PropertyObject) : obj -> obj =
            match tryGetGetter o with
            | Some f -> f
            | None -> fun o -> failwith ("Property does not contain getter")

        let getSetter (o:PropertyObject) : obj -> obj -> unit =
            match tryGetSetter o with
            | Some f -> f
            | None -> fun s o -> failwith ("Property does not contain setter")

        let containsGetter (o:PropertyObject) : bool =
            match tryGetGetter o with
            | Some _ -> true
            | None -> false

        let containsSetter (o:PropertyObject) : bool =
            match tryGetSetter o with
            | Some _ -> true
            | None -> false

        let isWritable (o:PropertyObject) : bool =
            containsSetter o

        [<Emit("isinstance($0, property)")>]
        let isProperty (o:obj) : bool =
            nativeOnly

        let tryProperty (o:obj) : PropertyObject option =
            if isProperty o then
                Some (o :?> PropertyObject)
            else
                None

    // Declared F# properties transpile to Python descriptors backed by fields.
    // Use getattr so reads go through the descriptor instead of Properties.
    [<Emit("getattr($0,$1)")>]
    let getStaticPropertyValue (o:obj) (propName:string) =
        nativeOnly

    // PropertyHelper stores delegates, so wrap descriptor access in a getter.
    let createStaticGetter (propName:string) =
        fun (o:obj) ->
            getStaticPropertyValue o propName

    // Use setattr so Python property setters update their backing fields.
    [<Emit("setattr($0,$1,$2)")>]
    let setStaticPropertyValue (o:obj) (propName:string) (value:obj) : unit =
        nativeOnly

    // PropertyHelper stores delegates, so wrap descriptor access in a setter.
    let createStaticSetter (propName:string) =
        fun (o:obj) (value:obj) ->
         setStaticPropertyValue o propName value

    // Dynamic properties must use DynamicObj.Properties.
    // Fable-generated backing fields also live in Python instance attributes.
    [<Emit("$0.Properties[$1]")>]
    let getPropertyValue (o:obj) (propName:string) =
        nativeOnly

    // Track DynamicObj-owned runtime mirrors outside the object so user keys cannot collide.
    // Use id-based storage because DynamicObj has structural equality/hash in Python.
    // The value is an id(obj) -> set(propertyName) map of names mirrored by DynamicObj.
    [<Emit("""globals().setdefault("_dynamicObjMirrorSets", {})""")>]
    let getMirrorSets () : obj =
        nativeOnly

    // Keep cleanup callbacks alive until the mirrored object is collected.
    [<Emit("""globals().setdefault("_dynamicObjMirrorFinalizers", {})""")>]
    let getMirrorFinalizers () : obj =
        nativeOnly

    // Check whether this object already has mirror ownership metadata.
    [<Emit("""id($1) in $0""")>]
    let mirrorStoreHas (store: obj) (o: obj) : bool =
        nativeOnly

    // Create ownership metadata and register cleanup for the id-based mirror entry.
    [<Emit("""($0.setdefault(id($2), __import__("weakref").finalize($2, lambda oid=id($2): ($1.pop(oid, None), $0.pop(oid, None)))), $1.setdefault(id($2), set()))""")>]
    let mirrorStoreCreate (finalizers: obj) (store: obj) (o: obj) : unit =
        nativeOnly

    // Only names in this set may be removed from the runtime object later.
    [<Emit("""id($1) in $0 and $2 in $0[id($1)]""")>]
    let mirrorStoreHasProperty (store: obj) (o: obj) (propName: string) : bool =
        nativeOnly

    // Mark a runtime attribute as owned by DynamicObj's compatibility mirror.
    [<Emit("""$0[id($1)].add($2)""")>]
    let mirrorStoreAddProperty (store: obj) (o: obj) (propName: string) : unit =
        nativeOnly

    // Unmark ownership after removing a DynamicObj-owned runtime mirror.
    [<Emit("""id($1) in $0 and $0[id($1)].discard($2)""")>]
    let mirrorStoreDeleteProperty (store: obj) (o: obj) (propName: string) : unit =
        nativeOnly

    let hasMirrorSet (o: obj) =
        mirrorStoreHas (getMirrorSets ()) o

    let createMirrorSet (o: obj) =
        mirrorStoreCreate (getMirrorFinalizers ()) (getMirrorSets ()) o

    let ensureMirrorSet (o: obj) =
        if not (hasMirrorSet o) then
            createMirrorSet o

    let isMirroredProperty (o: obj) (propName: string) =
        mirrorStoreHasProperty (getMirrorSets ()) o propName

    let markMirroredProperty (o: obj) (propName: string) =
        mirrorStoreAddProperty (getMirrorSets ()) o propName

    let unmarkMirroredProperty (o: obj) (propName: string) =
        mirrorStoreDeleteProperty (getMirrorSets ()) o propName

    [<Emit("hasattr($0, $1)")>]
    let hasRuntimeProperty (o: obj) (propName: string) : bool =
        nativeOnly

    [<Emit("setattr($0, $1, $2)")>]
    let setRuntimeProperty (o: obj) (propName: string) (value: obj) : unit =
        nativeOnly

    [<Emit("delattr($0, $1)")>]
    let deleteRuntimeProperty (o: obj) (propName: string) : unit =
        nativeOnly

    // Mirror only new names, or names already owned by our mirror; never overwrite typed/runtime members.
    let shouldMirrorDynamicProperty (o: obj) (propName: string) =
        isMirroredProperty o propName || not (hasRuntimeProperty o propName)

    // Keep native Python access like obj.extension working while Properties remains authoritative.
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

    let createGetter (propName:string) =
        fun (o:obj) ->
            getPropertyValue o propName

    // Keep SetProperty aligned with .NET by writing dynamic values to Properties.
    [<Emit("$0.Properties[$1] = $2")>]
    let setStoredPropertyValue (o:obj) (propName:string) (value:obj) : unit =
        nativeOnly

    // Dynamic SetProperty writes to Properties first, then updates the optional native mirror.
    let setPropertyValue (o:obj) (propName:string) (value:obj) : unit =
        setStoredPropertyValue o propName value
        mirrorDynamicProperty o propName value

    let createSetter (propName:string) =
        fun (o:obj) (value:obj) ->
         setPropertyValue o propName value


    [<Emit("vars($0)")>]
    let getOwnMemberObjects (o:obj) : Dictionary<string,obj> =
        nativeOnly

    // Enumerate only explicit dynamic properties, not generated instance fields.
    [<Emit("$0.Properties")>]
    let getDynamicMemberObjects (o:obj) : Dictionary<string,obj> =
        nativeOnly

    [<Emit("$0.__class__")>]
    let getClass (o:obj) : obj =
        nativeOnly

    let getStaticPropertyObjects (o:obj) : Dictionary<string,PropertyObject> =
        getClass o
        |> getOwnMemberObjects
        |> Seq.choose (fun kv ->
            kv.Value
            |> PropertyObject.tryProperty
            |> Option.map (fun po -> KeyValuePair(kv.Key, po))
        )
        |> Dictionary.ofSeq

    let removeStaticPropertyValue (o:obj) (propName:string) =
        setStaticPropertyValue o propName null

    [<Emit("$0.Properties.pop($1, None)")>]
    let deleteStoredPropertyValue (o:obj) (propName:string) : unit =
        nativeOnly

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



    [<Emit("$1 in $0.__dict__")>]
    let hasOwnMemberObject (o:obj) (propName:string) : bool =
        nativeOnly

    [<Emit("$0.__dict__[$1]")>]
    let getOwnMemberObject (o:obj) (propName:string) =
        nativeOnly

    // Dynamic lookup checks Properties so backing fields never count as members.
    [<Emit("$1 in $0.Properties")>]
    let hasMemberObject (o:obj) (propName:string) : bool =
        nativeOnly

    [<Emit("$0.Properties[$1]")>]
    let getMemberObject (o:obj) (propName:string) =
        nativeOnly

    let tryGetPropertyObject (o:obj) (propName:string) : PropertyObject option =
        let memberObject =
            if hasOwnMemberObject o propName then
                getOwnMemberObject o propName
            else
                null
        match PropertyObject.tryProperty memberObject with
        | Some po -> Some po
        | None -> None

    let tryGetDynamicPropertyHelper (o:obj) (propName:string) : PropertyHelper option =
        if hasMemberObject o propName then
            Some {
                Name = propName
                IsStatic = false
                IsDynamic = true
                IsMutable = true
                IsImmutable = false
                GetValue = createGetter propName
                SetValue = createSetter propName
                RemoveValue = fun o -> deleteDynamicPropertyValue o propName
            }
        else
            None

    let tryGetStaticPropertyHelper (o:obj) (propName:string) : PropertyHelper option =
        match tryGetPropertyObject (getClass o) propName with
        | Some po -> 
            let isWritable = PropertyObject.isWritable po
            Some {
                Name = propName
                IsStatic = true
                IsDynamic = false
                IsMutable = isWritable
                IsImmutable = not isWritable
                GetValue = createStaticGetter propName
                SetValue = createStaticSetter propName
                RemoveValue = fun o -> removeStaticPropertyValue o propName
            }
         | None -> None

    let getDynamicPropertyHelpers (o:obj) : PropertyHelper [] =
        getDynamicMemberObjects o
        |> Seq.choose (fun kv -> 
            let n = kv.Key
            {
                Name = n
                IsStatic = false
                IsDynamic = true
                IsMutable = true
                IsImmutable = false
                GetValue = createGetter n
                SetValue = createSetter n
                RemoveValue = fun o -> deleteDynamicPropertyValue o n
            }
            |> Some
        )
        |> Seq.toArray


    let getStaticPropertyHelpers (o:obj) : PropertyHelper [] =
        getStaticPropertyObjects o
        |> Seq.map (fun kv -> 
            let n = kv.Key
            let po = kv.Value
            {
                Name = n
                IsStatic = true
                IsDynamic = false
                IsMutable = PropertyObject.isWritable po
                IsImmutable = not (PropertyObject.isWritable po)
                GetValue = createStaticGetter n
                SetValue = createStaticSetter n
                RemoveValue = fun o -> removeStaticPropertyValue o n
            }
        )
        |> Seq.toArray

    let getPropertyHelpers (o:obj) =
        getDynamicPropertyHelpers o
        |> Array.append (getStaticPropertyHelpers o)

    let getPropertyNames (o:obj) =
        getPropertyHelpers o 
        |> Array.map (fun h -> h.Name)

    // Used by ofDict because assigning Properties directly bypasses SetProperty mirroring.
    let syncRuntimeDynamicProperties (o: obj) =
        getDynamicMemberObjects o
        |> Seq.iter (fun kv -> mirrorDynamicProperty o kv.Key kv.Value)

    module Interfaces = 
        
        [<Emit("""hasattr($0, 'System_ICloneable_Clone') and callable($0.System_ICloneable_Clone)""")>]
        let implementsICloneable (o:obj) : bool =
            nativeOnly

        [<Emit("""$0.System_ICloneable_Clone()""")>]
        let cloneICloneable (o:obj) : obj =
            nativeOnly

    module Dictionaries =
        [<Emit("""isinstance($0, dict)""")>]
        let isDict (o:obj) : bool =
            nativeOnly

    module Collections =
        [<Emit("""isinstance($0, list)""")>]
        let isList (o:obj) : bool =
            nativeOnly

#endif
