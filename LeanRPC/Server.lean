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
  let mkErrorResponse (code : Int) (message : String) (id : JsonRPCID) (data? : Option Lean.Json := none) : String :=
    (Lean.toJson (JsonRPCResponse.error code message id data?)).compress

  match Lean.Json.parse jsonRequest with
  | .error err => pure (mkErrorResponse JsonRPCErrorCode.parseError s!"Parse error: {err}" JsonRPCID.null)
  | .ok requestJson =>
    match Lean.fromJson? requestJson with
    | .error err => pure (mkErrorResponse JsonRPCErrorCode.invalidRequest s!"Invalid request: {err}" JsonRPCID.null)
    | .ok (req : JsonRPCRequest) =>
      let id := req.id?.getD JsonRPCID.null
      match JsonRPCRequest.validate req with
      | .error errObj => pure (mkErrorResponse errObj.code errObj.message id errObj.data?)
      | .ok _ =>
        match registry.get? req.method with
        | none => pure (mkErrorResponse JsonRPCErrorCode.methodNotFound s!"Method '{req.method}' not found" id)
        | some handler => do
          let response ← handler req.params? id
          pure (Lean.toJson response).compress

def launchRPCServer (config : ServerConfig) (registryBuilder : MethodRegistry → MethodRegistry) : IO (IO Unit) := do
  let registry := withBuiltinMethods (registryBuilder mkMethodRegistry)
  let jsonHandler := createJsonRPCHandler registry

  let stopFlag ← IO.mkRef false
  let _serverTask ← IO.asTask (startJsonRPCServer config jsonHandler stopFlag)
  pure (stopFlag.set true)

def startRPCServer (config : ServerConfig) (registryBuilder : MethodRegistry → MethodRegistry) : IO Unit := do
  let _stopServer ← launchRPCServer config registryBuilder

  if config.logging then
    IO.println "Press Ctrl+C to stop the server."

  let waitTask ← IO.asTask (IO.sleep 2147483647)
  let _ ← IO.wait waitTask

end LeanRPC.Server
