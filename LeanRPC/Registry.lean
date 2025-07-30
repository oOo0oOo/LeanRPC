import LeanRPC.Protocol
import LeanSerde
import Lean.Elab
import Lean
import Std.Data.HashMap

open Lean Elab
open LeanRPC.Protocol

namespace LeanRPC.Registry

abbrev MethodHandler := Option Lean.Json → JsonRPCID → IO (Except String JsonRPCResponse)

class HandlerBuilder (α : Type) where
  build : α → List Lean.Json → IO (Except String Lean.Json)

-- Pure and relatively simple monads
instance [LeanSerde.Serializable β] : HandlerBuilder β where
  build f params := do
    if params.isEmpty then
      pure (.ok (LeanSerde.serialize f))
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable β] : HandlerBuilder (IO β) where
  build f params := do
    if params.isEmpty then
      try
        let result ← f
        pure (.ok (LeanSerde.serialize result))
      catch e =>
        pure (.error s!"IO function execution failed: {e}")
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable ε] [LeanSerde.Serializable β] : HandlerBuilder (Except ε β) where
  build f params := do
    if params.isEmpty then
      match f with
      | .ok result => pure (.ok (LeanSerde.serialize result))
      | .error err => pure (.error s!"Function returned error: {(LeanSerde.serialize err : String)}")
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable β] : HandlerBuilder (Option β) where
  build f params := do
    if params.isEmpty then
      match f with
      | .some result => pure (.ok (LeanSerde.serialize result))
      | .none => pure (.error "Function returned None")
    else
      pure (.error "Too many parameters")

-- Sandboxed monads (only pure computation without current context)
instance [LeanSerde.Serializable β] : HandlerBuilder (Lean.Elab.Command.CommandElabM β) where
  build f params := do
    if params.isEmpty then
      try
        let env ← Lean.importModules #[] {} 0
        let commandCtx : Command.Context := {
          fileName := "<rpc>",
          fileMap := default,
          snap? := none,
          cancelTk? := none,
        }
        let commandState : Command.State := Command.mkState env
        let eio := (f.run commandCtx).run commandState
        match ← eio.toIO' with
        | .ok (result, _) => pure (.ok (LeanSerde.serialize result))
        | .error _ => pure (.error "CommandElabM execution failed")
      catch e =>
        pure (.error s!"CommandElabM function execution failed: {e}")
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable β] : HandlerBuilder (Lean.CoreM β) where
  build f params := do
    if params.isEmpty then
      try
        let commandAction : Command.CommandElabM β := Command.liftCoreM f
        let env ← Lean.importModules #[] {} 0
        let commandCtx : Command.Context := {
          fileName := "<rpc>",
          fileMap := default,
          snap? := none,
          cancelTk? := none,
        }
        let commandState : Command.State := Command.mkState env
        let eio := (commandAction.run commandCtx).run commandState
        match ← eio.toIO' with
        | .ok (result, _) => pure (.ok (LeanSerde.serialize result))
        | .error _ => pure (.error s!"CoreM execution failed")
      catch e =>
        pure (.error s!"CoreM function execution failed: {e}")
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable β] : HandlerBuilder (Lean.MetaM β) where
  build f params := do
    if params.isEmpty then
      try
        let env ← Lean.importModules #[] {} 0
        let coreCtx : Lean.Core.Context := {
          fileName := "<rpc>",
          fileMap := default,
          maxHeartbeats := 0,
          maxRecDepth := 1000,
          openDecls := [],
          initHeartbeats := 0
        }
        let coreState : Lean.Core.State := { env := env }
        let metaCtx : Lean.Meta.Context := {}
        let metaState : Lean.Meta.State := {}
        let eio := (f.run metaCtx metaState |>.run coreCtx coreState)
        match ← eio.toIO' with
        | .ok ((result, _), _) => pure (.ok (LeanSerde.serialize result))
        | .error _ => pure (.error s!"MetaM execution failed")
        catch e =>
        pure (.error s!"MetaM function execution failed: {e}")
    else
      pure (.error "Too many parameters")

instance [LeanSerde.Serializable β] : HandlerBuilder (Lean.Elab.TermElabM β) where
  build f params := do
    if params.isEmpty then
      try
        let commandAction : Command.CommandElabM β := Command.liftTermElabM f
        let env ← Lean.importModules #[] {} 0
        let commandCtx : Command.Context := {
          fileName := "<rpc>",
          fileMap := default,
          snap? := none,
          cancelTk? := none,
        }
        let commandState : Command.State := Command.mkState env
        let eio := (commandAction.run commandCtx).run commandState
        match ← eio.toIO' with
        | .ok (result, _) => pure (.ok (LeanSerde.serialize result))
        | .error _ => pure (.error s!"TermElabM execution failed")
      catch e =>
        pure (.error s!"TermElabM function execution failed: {e}")
    else
      pure (.error "Too many parameters")

-- StateT & ReaderT IO support
instance [LeanSerde.Serializable σ] [LeanSerde.Serializable β] : HandlerBuilder (StateT σ IO β) where
  build f params := do
    match params with
    | [] => pure (.error "StateT IO requires initial state parameter")
    | stateParam :: restParams => do
      if !restParams.isEmpty then
        pure (.error "StateT IO: too many parameters after state")
      else
        match LeanSerde.deserialize stateParam with
        | .ok initialState => do
          try
            let (result, finalState) ← f.run initialState
            let resultTuple := (result, finalState)
            pure (.ok (LeanSerde.serialize resultTuple))
          catch e =>
            pure (.error s!"StateT IO execution failed: {e}")
        | .error err => pure (.error s!"State parameter deserialization failed: {err}")

instance [LeanSerde.Serializable ρ] [LeanSerde.Serializable β] : HandlerBuilder (ReaderT ρ IO β) where
  build f params := do
    match params with
    | [] => pure (.error "ReaderT IO requires environment parameter")
    | envParam :: restParams => do
      if !restParams.isEmpty then
        pure (.error "ReaderT IO: too many parameters after environment")
      else
        match LeanSerde.deserialize envParam with
        | .ok environment => do
          try
            let result ← f.run environment
            pure (.ok (LeanSerde.serialize result))
          catch e =>
            pure (.error s!"ReaderT IO execution failed: {e}")
        | .error err => pure (.error s!"Environment parameter deserialization failed: {err}")

-- Various
instance [LeanSerde.Serializable β] : HandlerBuilder (Task β) where
  build f params := do
    if params.isEmpty then
      try
        let result := f.get
        pure (.ok (LeanSerde.serialize result))
      catch e =>
        pure (.error s!"Task execution failed: {e}")
    else
      pure (.error "Too many parameters")

-- Function types (recursive case)
instance [LeanSerde.Serializable α] [HandlerBuilder β] : HandlerBuilder (α → β) where
  build f params := do
    match params with
    | [] => pure (.error "Not enough parameters")
    | p::ps => do
      match LeanSerde.deserialize p with
      | .ok arg => HandlerBuilder.build (f arg) ps
      | .error err => pure (.error s!"Parameter deserialization failed: {err}")

class ToHandler (α : Type) where
  toHandler : α → MethodHandler

instance [HandlerBuilder α] : ToHandler α where
  toHandler f := fun params? id => do
    let paramsList := match params? with
      | none => []
      | some (.arr arr) => arr.toList
      | some other => [other]

    match ← HandlerBuilder.build f paramsList with
    | .ok val => pure (.ok (JsonRPCResponse.success val id))
    | .error msg =>
      let errCode := if msg.startsWith "Too many" || msg.startsWith "Not enough"
                      then JsonRPCErrorCode.invalidParams
                      else JsonRPCErrorCode.internalError
      pure (.ok (JsonRPCResponse.error errCode msg id))

def createHandler {α : Type} [ToHandler α] (f : α) : MethodHandler := ToHandler.toHandler f

abbrev MethodRegistry := Std.HashMap String MethodHandler

def mkMethodRegistry : MethodRegistry := Std.HashMap.emptyWithCapacity 16

def registerMethod (registry : MethodRegistry) (method : String) (handler : MethodHandler) : MethodRegistry :=
  registry.insert method handler

def registerFunction {α : Type} [ToHandler α] (registry : MethodRegistry) (method : String) (f : α) : MethodRegistry :=
  registry.insert method (createHandler f)

-- Built-in methods
def listMethods (registry : MethodRegistry) : List String :=
  registry.toList.map (·.1)

def withBuiltinMethods (registry : MethodRegistry) : MethodRegistry :=
  let listMethodsHandler : MethodHandler := fun _ id => do
    let methods := listMethods registry
    let methodsJson := Lean.Json.arr (methods.map Lean.Json.str).toArray
    pure (.ok (JsonRPCResponse.success methodsJson id))
  registry.insert "list_methods" listMethodsHandler

end LeanRPC.Registry
