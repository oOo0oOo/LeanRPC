import LeanRPC.Server
import LeanRPC.Registry
import LeanRPC.Protocol
import LeanRPC.Attribute
import LeanRPC.HTTP
import Tests.TestFramework
import Tests.AttributeTests
import Lean.Data.Json
import LeanSerde

namespace LeanRPC.Tests.Server

open LeanRPC.Tests
open LeanRPC.Server
open LeanRPC.Registry
open LeanRPC.Protocol
open LeanRPC.HTTP

-- Test createJsonRPCHandler with valid requests
def testCreateJsonRPCHandlerValid (_ : Unit) : IO TestResult := do
  let registry := mkMethodRegistry
  let addFunc : Nat → Nat → IO Nat := fun a b => pure (a + b)
  let registryWithAdd := registerFunction registry "add" addFunc

  let handler := createJsonRPCHandler registryWithAdd

  -- Test valid JSON-RPC request
  let param1 := LeanSerde.serialize (5 : Nat)
  let param2 := LeanSerde.serialize (3 : Nat)
  let params := Lean.Json.arr #[param1, param2]
  let request := "{\"jsonrpc\":\"2.0\",\"method\":\"add\",\"params\":" ++ params.compress ++ ",\"id\":1}"

  let responseStr ← handler request

  match Lean.Json.parse responseStr with
  | .error err => return assert false s!"Response parsing failed: {err}"
  | .ok responseJson =>
    match Lean.fromJson? responseJson with
    | .error err => return assert false s!"Response deserialization failed: {err}"
    | .ok (response : JsonRPCResponse) =>
      match response.result? with
      | some result =>
        match LeanSerde.deserialize result with
        | .ok (8 : Nat) => return assert true "Valid JSON-RPC request handled correctly"
        | .ok val => return assert false s!"Wrong result: {val}"
        | .error err => return assert false s!"Result deserialization failed: {err}"
      | none => return assert false "No result in response"

-- Test createJsonRPCHandler with parse error
def testCreateJsonRPCHandlerParseError (_ : Unit) : IO TestResult := do
  let registry := mkMethodRegistry
  let handler := createJsonRPCHandler registry

  let invalidJson := "{invalid json"
  let responseStr ← handler invalidJson

  match Lean.Json.parse responseStr with
  | .error err => return assert false s!"Response parsing failed: {err}"
  | .ok responseJson =>
    match Lean.fromJson? responseJson with
    | .error err => return assert false s!"Response deserialization failed: {err}"
    | .ok (response : JsonRPCResponse) =>
      match response.error? with
      | some error =>
        if error.code == JsonRPCErrorCode.parseError then
          return assert true "Parse error handled correctly"
        else
          return assert false s!"Wrong error code: {error.code}"
      | none => return assert false "Expected error response"

-- Test createJsonRPCHandler with invalid request
def testCreateJsonRPCHandlerInvalidRequest (_ : Unit) : IO TestResult := do
  let registry := mkMethodRegistry
  let handler := createJsonRPCHandler registry

  -- Valid JSON but invalid JSON-RPC request (missing jsonrpc field)
  let invalidRequest := "{\"method\":\"test\",\"id\":1}"
  let responseStr ← handler invalidRequest

  match Lean.Json.parse responseStr with
  | .error err => return assert false s!"Response parsing failed: {err}"
  | .ok responseJson =>
    match Lean.fromJson? responseJson with
    | .error err => return assert false s!"Response deserialization failed: {err}"
    | .ok (response : JsonRPCResponse) =>
      match response.error? with
      | some error =>
        if error.code == JsonRPCErrorCode.invalidRequest then
          return assert true "Invalid request handled correctly"
        else
          return assert false s!"Wrong error code: {error.code}"
      | none => return assert false "Expected error response"

-- Test createJsonRPCHandler with method not found
def testCreateJsonRPCHandlerMethodNotFound (_ : Unit) : IO TestResult := do
  let registry := mkMethodRegistry
  let handler := createJsonRPCHandler registry

  let request := "{\"jsonrpc\":\"2.0\",\"method\":\"nonexistent\",\"id\":1}"
  let responseStr ← handler request

  match Lean.Json.parse responseStr with
  | .error err => return assert false s!"Response parsing failed: {err}"
  | .ok responseJson =>
    match Lean.fromJson? responseJson with
    | .error err => return assert false s!"Response deserialization failed: {err}"
    | .ok (response : JsonRPCResponse) =>
      match response.error? with
      | some error =>
        if error.code == JsonRPCErrorCode.methodNotFound then
          return assert true "Method not found handled correctly"
        else
          return assert false s!"Wrong error code: {error.code}"
      | none => return assert false "Expected error response"

-- Test createJsonRPCHandler with request validation error
def testCreateJsonRPCHandlerValidationError (_ : Unit) : IO TestResult := do
  let registry := mkMethodRegistry
  let handler := createJsonRPCHandler registry

  -- Request with invalid jsonrpc version
  let request := "{\"jsonrpc\":\"1.0\",\"method\":\"test\",\"id\":1}"
  let responseStr ← handler request

  match Lean.Json.parse responseStr with
  | .error err => return assert false s!"Response parsing failed: {err}"
  | .ok responseJson =>
    match Lean.fromJson? responseJson with
    | .error err => return assert false s!"Response deserialization failed: {err}"
    | .ok (response : JsonRPCResponse) =>
      match response.error? with
      | some error =>
        -- Should be an invalid request error due to wrong version
        if error.code == JsonRPCErrorCode.invalidRequest then
          return assert true "Request validation error handled correctly"
        else
          return assert false s!"Wrong error code: {error.code}"
      | none => return assert false "Expected error response"


-- Helper function to extract response body from HTTP response
def extractResponseBody (httpResponse : String) : Except String String := do
  let parts := httpResponse.splitOn "\r\n\r\n"
  if parts.length < 2 then throw "Invalid HTTP response: missing body"
  let body := parts.drop 1 |> String.intercalate "\r\n\r\n"
  return body

-- Test the real use case with startRPCServer
def testRealUseCaseIntegration (_ : Unit) : IO TestResult := do
  let config : ServerConfig := {
    port := 8093,
    host := "127.0.0.1",
    maxBodySize := 1024 * 1024,
    logging := false
  }

  -- Start the RPC server in a separate task
  let stopServer ← LeanRPC.Server.launchRPCServer config buildRPC

  -- Give server time to start
  IO.sleep 500

  try
    -- Test 1: Use testRPCAdd function from AttributeTests (available via init_RPC)
    let param1 := LeanSerde.serialize (42 : Nat)
    let param2 := LeanSerde.serialize (17 : Nat)
    let params := Lean.Json.arr #[param1, param2]
    let addRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"testRPCAdd\",\"params\":" ++ params.compress ++ ",\"id\":1}"

    let addSuccess ← match ← makeJsonHttpRequest config addRequest with
    | .ok response =>
      match extractResponseBody response with
      | .ok body =>
        match Lean.Json.parse body with
        | .ok json =>
          match Lean.fromJson? json with
          | .ok (resp : LeanRPC.Protocol.JsonRPCResponse) =>
            match resp.result? with
            | some result =>
              match LeanSerde.deserialize result with
              | .ok (59 : Nat) => pure true  -- 42 + 17 = 59
              | _ => pure false
            | none => pure false
          | .error _ => pure false
        | .error _ => pure false
      | .error _ => pure false
    | .error _ => pure false

    -- Test 2: Use testRPCNumCombos function from AttributeTests
    let names := LeanSerde.serialize ["Alice", "Bob"]
    let cats := LeanSerde.serialize [6, 66, 666]
    let combosParams := Lean.Json.arr #[names, cats]
    let combosRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"testRPCNumCombos\",\"params\":" ++ combosParams.compress ++ ",\"id\":2}"

    let combosSuccess ← match ← makeJsonHttpRequest config combosRequest with
    | .ok response =>
      match extractResponseBody response with
      | .ok body =>
        match Lean.Json.parse body with
        | .ok json =>
          match Lean.fromJson? json with
          | .ok (resp : LeanRPC.Protocol.JsonRPCResponse) =>
            match resp.result? with
            | some result =>
              match LeanSerde.deserialize result with
              | .ok (6 : Nat) => pure true  -- 2 * 3 = 6
              | _ => pure false
            | none => pure false
          | .error _ => pure false
        | .error _ => pure false
      | .error _ => pure false
    | .error _ => pure false

    -- Test 3: Built-in rpc_listMethods
    let listRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"rpc_listMethods\",\"id\":3}"

    let listSuccess ← match ← makeJsonHttpRequest config listRequest with
    | .ok response =>
      match extractResponseBody response with
      | .ok body =>
        match Lean.Json.parse body with
        | .ok json =>
          match Lean.fromJson? json with
          | .ok (resp : LeanRPC.Protocol.JsonRPCResponse) =>
            match resp.result? with
            | some result =>
              match LeanSerde.deserialize result with
              | .ok (methods : List String) =>
                pure (methods.contains "testRPCAdd" && methods.contains "testRPCNumCombos" && methods.contains "rpc_listMethods")
              | _ => pure false
            | none => pure false
          | .error _ => pure false
        | .error _ => pure false
      | .error _ => pure false
    | .error _ => pure false

    -- Collect results
    let allTests := [
      ("testRPCAdd function", addSuccess),
      ("testRPCNumCombos function", combosSuccess),
      ("List methods", listSuccess)
    ]

    let failedTests := allTests.filter (fun (_, success) => !success)

    if failedTests.isEmpty then
      return assert true "Real use case integration test passed"
    else
      let failureNames := failedTests.map (fun (name, _) => name)
      return assert false s!"Real use case test failed on: {failureNames}"

  catch e =>
    return assert false s!"HTTP request failed: {e}"
  finally
    stopServer

-- Run all server tests
def runServerTests : IO Unit := do
  runTests #[
    ("CreateJsonRPCHandler Valid", testCreateJsonRPCHandlerValid),
    ("CreateJsonRPCHandler Parse Error", testCreateJsonRPCHandlerParseError),
    ("CreateJsonRPCHandler Invalid Request", testCreateJsonRPCHandlerInvalidRequest),
    ("CreateJsonRPCHandler Method Not Found", testCreateJsonRPCHandlerMethodNotFound),
    ("CreateJsonRPCHandler Validation Error", testCreateJsonRPCHandlerValidationError),
    ("Real Use Case Integration", testRealUseCaseIntegration)
  ]

end LeanRPC.Tests.Server
