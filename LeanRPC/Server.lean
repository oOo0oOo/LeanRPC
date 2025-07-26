import LeanRPC.HTTP
import LeanRPC.Protocol
import LeanRPC.Registry
import LeanRPC.Attribute
import Lean.Data.Json

namespace LeanRPC.Server

open LeanRPC.HTTP
open LeanRPC.Protocol
open LeanRPC.Registry

def createJsonRpcHandler (registry : MethodRegistry) : String → IO String := fun jsonRequest => do
  match Lean.Json.parse jsonRequest with
  | .error err =>
    let response := JsonRPCResponse.error JsonRPCErrorCode.parseError s!"Parse error: {err}" JsonRPCID.null
    pure (Lean.toJson response).compress
  | .ok requestJson =>
    match Lean.fromJson? requestJson with
    | .error err =>
      let response := JsonRPCResponse.error JsonRPCErrorCode.invalidRequest s!"Invalid request: {err}" JsonRPCID.null
      pure (Lean.toJson response).compress
    | .ok (req : JsonRPCRequest) =>
      let id := req.id?.getD JsonRPCID.null
      match JsonRPCRequest.validate req with
      | .error errObj =>
        let response := JsonRPCResponse.error errObj.code errObj.message id errObj.data?
        pure (Lean.toJson response).compress
      | .ok _ =>
        match registry.get? req.method with
        | none =>
          let response := JsonRPCResponse.error JsonRPCErrorCode.methodNotFound s!"Method '{req.method}' not found" id
          pure (Lean.toJson response).compress
        | some handler => do
          let response ← handler req.params? id
          pure (Lean.toJson response).compress

def startRPCServer (config : ServerConfig) (builder : MethodRegistry → MethodRegistry) : IO (IO Unit) := do
  let baseRegistry := mkMethodRegistry
  let registry := builder baseRegistry
  let fullRegistryRef ← IO.mkRef (Option.none : Option MethodRegistry)

  -- Add built-in methods
  let listMethods : IO (List String) := do
    match ← fullRegistryRef.get with
    | none => pure []
    | some r => pure (rpc_listMethods r)

  let fullRegistry := registerFunction registry "rpc_listMethods" listMethods
  fullRegistryRef.set (some fullRegistry)

  let jsonHandler := createJsonRpcHandler fullRegistry
  let stopFlag ← IO.mkRef false
  let serverTask ← IO.asTask (startJsonRpcServer { port := config.port, host := config.host, maxBodySize := config.maxBodySize } jsonHandler stopFlag)

  let stopServer : IO Unit := do
    stopFlag.set true
    IO.cancel serverTask

  pure stopServer

end LeanRPC.Server
