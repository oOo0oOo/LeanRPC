import LeanRPC.HTTP
import LeanRPC.Protocol
import LeanSerde
import Lean.Data.Json

namespace LeanRPC.Client

open LeanRPC.HTTP
open LeanRPC.Protocol

private def extractResponseBody (httpResponse : String) : Except String String :=
  let parts := httpResponse.splitOn "\r\n\r\n"
  if parts.length < 2 then
    .error "Invalid HTTP response: missing body"
  else
    .ok (parts.drop 1 |> String.intercalate "\r\n\r\n")

def callRPC [LeanSerde.Serializable β]
    (config : ServerConfig) (method : String) (params : List Lean.Json) (id : JsonRPCID := JsonRPCID.num 1) : IO (Except String β) := do
  try
    let request : JsonRPCRequest := {
      method := method,
      params? := some (Lean.Json.arr params.toArray),
      id? := id
    }
    let requestJson := (Lean.toJson request).compress
    let httpResult ← makeJsonHttpRequest config requestJson

    let result : Except String β := do
      let responseBody ← httpResult.bind extractResponseBody
      let jsonResponse ← Lean.Json.parse responseBody
      let rpcResponse : JsonRPCResponse ← Lean.fromJson? jsonResponse

      if rpcResponse.id != id then
        throw s!"Response ID mismatch. Expected {Lean.toJson id}, got {Lean.toJson rpcResponse.id}."

      if let some err := rpcResponse.error? then
        throw s!"RPC error ({err.code}): {err.message}"

      match rpcResponse.result? with
      | none => throw "No result in response"
      | some resultJson =>
        LeanSerde.deserialize resultJson

    return result
  catch e =>
    return .error e.toString

end LeanRPC.Client
