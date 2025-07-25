import LeanRPC.Protocol
import LeanSerial
import Std.Data.HashMap

namespace LeanRPC.Registry

open LeanRPC.Protocol

abbrev MethodHandler := Option Lean.Json → JsonRPCID → IO JsonRPCResponse

def Except.toIO {α : Type} : Except String α → IO α
  | .ok a => pure a
  | .error msg => throw $ IO.userError msg

class HandlerBuilder (α : Type) where
  build : α → List Lean.Json → IO Lean.Json

instance [LeanSerial.Serializable β] : HandlerBuilder (IO β) where
  build f params :=
    if params.isEmpty then
      LeanSerial.serialize <$> f
    else
      throw $ IO.userError "Too many parameters"

instance [LeanSerial.Serializable α] [HandlerBuilder β] : HandlerBuilder (α → β) where
  build f params :=
    match params with
    | [] => throw $ IO.userError "Not enough parameters"
    | p::ps => do
      let arg ← Except.toIO (LeanSerial.deserialize p)
      HandlerBuilder.build (f arg) ps

class ToHandler (α : Type) where
  toHandler : α → MethodHandler

instance [HandlerBuilder α] : ToHandler α where
  toHandler f := fun params? id => do
    let result ←
      try
        let paramsList := match params? with
          | none => []
          | some (.arr arr) => arr.toList
          | some other => [other]
        Except.ok <$> HandlerBuilder.build f paramsList
      catch e =>
        pure $ Except.error e.toString

    match result with
    | .ok val => return JsonRPCResponse.success val id
    | .error msg =>
      let errCode := if msg.startsWith "Too many" || msg.startsWith "Not enough"
                      then JsonRPCErrorCode.invalidParams
                      else JsonRPCErrorCode.internalError
      return JsonRPCResponse.error errCode msg id

def createHandler {α : Type} [ToHandler α] (f : α) : MethodHandler := ToHandler.toHandler f

abbrev MethodRegistry := Std.HashMap String MethodHandler

def registerMethod (registry : MethodRegistry) (method : String) (handler : MethodHandler) : MethodRegistry :=
  registry.insert method handler

def registerFunction {α : Type} [ToHandler α] (registry : MethodRegistry) (method : String) (f : α) : MethodRegistry :=
  registry.insert method (createHandler f)

-- Built-in methods
def rpc_listMethods (registry : MethodRegistry) : List String :=
  registry.toList.map (·.1)

end LeanRPC.Registry
