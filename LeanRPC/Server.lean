import LeanRPC.HTTP
import LeanRPC.Protocol
import LeanRPC.Registry
import LeanRPC.Attribute
import Lean.Data.Json

namespace LeanRPC.Server

open LeanRPC.HTTP
open LeanRPC.Protocol
open LeanRPC.Registry

def createJsonRPCHandler (registry : MethodRegistry) : String → IO String := fun jsonRequest => do
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

def launchRPCServer (config : ServerConfig) (builder : MethodRegistry → MethodRegistry) : IO (IO Unit) := do
  let baseRegistry := mkMethodRegistry
  let registry := builder baseRegistry
  let fullRegistryRef ← IO.mkRef (Option.none : Option MethodRegistry)

  let listMethods : IO (List String) := do
    match ← fullRegistryRef.get with
    | none => pure []
    | some r => pure (rpc_listMethods r)

  let fullRegistry := registerFunction registry "rpc_listMethods" listMethods
  fullRegistryRef.set (some fullRegistry)

  let jsonHandler := createJsonRPCHandler fullRegistry
  let stopFlag ← IO.mkRef false
  let _serverTask ← IO.asTask (startJsonRPCServer config jsonHandler stopFlag)

  pure (stopFlag.set true)

def startRPCServer (config : ServerConfig) (builder : MethodRegistry → MethodRegistry) : IO Unit := do
  let _stopServer ← launchRPCServer config builder

  if config.logging then
    IO.println "Press Ctrl+C to stop the server."

  -- Simple blocking mechanism - wait indefinitely
  let waitTask ← IO.asTask (IO.sleep 2147483647)
  let _ ← IO.wait waitTask

end LeanRPC.Server
