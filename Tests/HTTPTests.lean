import LeanRPC.HTTP
import LeanRPC.Protocol
import LeanRPC.Registry
import Tests.TestFramework
import Lean.Data.Json
import Std.Internal.Async.TCP
import Std.Net.Addr

namespace LeanRPC.Tests.HTTP

open LeanRPC.Tests
open LeanRPC.HTTP
open LeanRPC.Protocol
open LeanRPC.Registry
open Std.Internal.IO.Async.TCP

-- Helpers
def stringContains (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

def extractResponseBody (httpResponse : String) : Except String String := do
  let parts := httpResponse.splitOn "\r\n\r\n"
  if parts.length < 2 then throw "Invalid HTTP response: missing body"
  let body := parts.drop 1 |> String.intercalate "\r\n\r\n"
  return body

-- Test JSON response formatting
def testJsonResponse (_ : Unit) : IO TestResult := do
  let body := "{\"result\":42}"
  let response := jsonResponse body

  if stringContains response "HTTP/1.1 200 OK" &&
     stringContains response "Content-Type: application/json" &&
     stringContains response body then
    return assert true "JSON response formatting works"
  else
    return assert false "JSON response formatting failed"

-- Test error response formatting
def testErrorResponse (_ : Unit) : IO TestResult := do
  let errorMsg := "Test error"
  let response := errorResponse errorMsg 400

  if stringContains response "HTTP/1.1 400" &&
     stringContains response "Content-Type: application/json" &&
     stringContains response "\"error\":\"Test error\"" then
    return assert true "Error response formatting works"
  else
    return assert false "Error response formatting failed"

-- Test JSON body extraction from valid POST request
def testExtractJsonBodyValid (_ : Unit) : IO TestResult := do
  let jsonBody := "{\"method\":\"test\",\"id\":1}"
  let httpRequest :=
    "POST /rpc HTTP/1.1\r\n" ++
    "Content-Type: application/json\r\n" ++
    "Content-Length: " ++ toString jsonBody.length ++ "\r\n" ++
    "\r\n" ++
    jsonBody

  match extractJsonBody httpRequest with
  | .ok body =>
    if body == jsonBody then
      return assert true "JSON body extraction works"
    else
      return assert false s!"Extracted body mismatch: expected '{jsonBody}', got '{body}'"
  | .error err =>
    return assert false s!"JSON body extraction failed: {err}"

-- Test rejection of non-POST requests
def testRejectNonPost (_ : Unit) : IO TestResult := do
  let httpRequest :=
    "GET /test HTTP/1.1\r\n" ++
    "Content-Length: 10\r\n" ++
    "\r\n" ++
    "{\"id\":1}"

  match extractJsonBody httpRequest with
  | .error err =>
    if stringContains err "POST" then
      return assert true "Non-POST requests correctly rejected"
    else
      return assert false s!"Wrong error message: {err}"
  | .ok _ =>
    return assert false "Non-POST request was incorrectly accepted"

-- Test missing content-length handling
def testMissingContentLength (_ : Unit) : IO TestResult := do
  let httpRequest :=
    "POST /rpc HTTP/1.1\r\n" ++
    "Content-Type: application/json\r\n" ++
    "\r\n" ++
    "{\"id\":1}"

  match extractJsonBody httpRequest with
  | .error err =>
    if stringContains err "Content-Length" then
      return assert true "Missing Content-Length correctly handled"
    else
      return assert false s!"Wrong error message: {err}"
  | .ok _ =>
    return assert false "Request without Content-Length was incorrectly accepted"

-- Test empty request handling
def testEmptyRequest (_ : Unit) : IO TestResult := do
  match extractJsonBody "" with
  | .error err =>
    if stringContains err "Invalid HTTP request" then
      return assert true "Empty request correctly handled"
    else
      return assert false s!"Wrong error message: {err}"
  | .ok _ =>
    return assert false "Empty request was incorrectly accepted"

-- Test server config defaults
def testServerConfigDefaults (_ : Unit) : IO TestResult := do
  let config : ServerConfig := {}
  if config.port == 8080 &&
     config.host == "127.0.0.1" &&
     config.maxBodySize == 1024 * 1024 then
    return assert true "Server config defaults are correct"
  else
    return assert false "Server config defaults are incorrect"

-- Test body length validation
def testBodyLengthMismatch (_ : Unit) : IO TestResult := do
  let jsonBody := "{\"method\":\"test\"}"
  let wrongLength := jsonBody.length + 5
  let httpRequest :=
    "POST /rpc HTTP/1.1\r\n" ++
    "Content-Length: " ++ toString wrongLength ++ "\r\n" ++
    "\r\n" ++
    jsonBody

  match extractJsonBody httpRequest with
  | .error err =>
    if stringContains err "Incomplete body" then
      return assert true "Body length mismatch correctly detected"
    else
      return assert false s!"Wrong error message: {err}"
  | .ok _ =>
    return assert false "Body length mismatch was not detected"

-- Test with a simple JSON handler
def testWithJsonHandler (_ : Unit) : IO TestResult := do
  let echoHandler : String → IO String := fun jsonInput => do
    pure ("{\"echo\":" ++ jsonInput ++ ",\"status\":\"success\"}")

  let jsonBody := "{\"method\":\"test\",\"id\":1}"
  let httpRequest :=
    "POST /rpc HTTP/1.1\r\n" ++
    "Content-Type: application/json\r\n" ++
    "Content-Length: " ++ toString jsonBody.length ++ "\r\n" ++
    "\r\n" ++
    jsonBody

  match extractJsonBody httpRequest with
  | .ok body => do
    let result ← echoHandler body
    let response := jsonResponse result

    -- Check that the response contains the expected elements
    if stringContains response "HTTP/1.1 200 OK" &&
       stringContains response "Content-Type: application/json" &&
       stringContains response "\"echo\":" &&
       stringContains response "\"status\":\"success\"" &&
       stringContains response jsonBody then
      return assert true "JSON handler integration works"
    else
      return assert false s!"JSON handler response malformed: {response}"
  | .error err =>
    return assert false s!"Failed to extract JSON body: {err}"

-- Test error handling with a failing handler
def testHandlerError (_ : Unit) : IO TestResult := do
  -- Handler that always throws an error
  let failingHandler : String → IO String := fun _ => do
    throw (IO.userError "Handler failed")

  let jsonBody := "{\"method\":\"test\",\"id\":1}"

  -- Simulate error handling
  let responseStr ← try
    let result ← failingHandler jsonBody
    pure (jsonResponse result)
  catch e =>
    pure (errorResponse e.toString 500)

  -- Check that error response is properly formatted
  if stringContains responseStr "HTTP/1.1 500" &&
     stringContains responseStr "\"error\":" &&
     stringContains responseStr "Handler failed" then
    return assert true "Handler error handling works"
  else
    return assert false s!"Handler error response malformed: {responseStr}"

-- Test full server integration with real JSON-RPC handlers using real HTTP calls
def testFullServerIntegration (_ : Unit) : IO TestResult := do
  -- Set up a test registry with some handlers
  let registry : MethodRegistry := mkMethodRegistry
  let addFunc : Nat → Nat → IO Nat := fun a b => pure (a + b)
  let getTimeFunc : IO String := pure "2024-07-26T12:00:00Z"
  let echoFunc : String → IO String := fun msg => pure s!"Echo: {msg}"

  let registryWithHandlers := registerFunction (registerFunction (registerFunction registry "add" addFunc) "getTime" getTimeFunc) "echo" echoFunc

  -- Create a JSON-RPC handler that processes requests
  let jsonRpcHandler : String → IO String := fun jsonRequest => do
    -- Parse the JSON-RPC request
    match Lean.Json.parse jsonRequest with
    | .error err => pure (Lean.toJson (JsonRPCResponse.error
        JsonRPCErrorCode.parseError
        s!"Parse error: {err}"
        JsonRPCID.null)).compress
    | .ok requestJson =>
      match Lean.fromJson? requestJson with
      | .error err => pure (Lean.toJson (JsonRPCResponse.error
          JsonRPCErrorCode.invalidRequest
          s!"Invalid request: {err}"
          JsonRPCID.null)).compress
      | .ok (req : JsonRPCRequest) => do
        -- Validate the request
        match req.validate with
        | .error errObj => pure (Lean.toJson (JsonRPCResponse.error
            errObj.code
            errObj.message
            (req.id?.getD JsonRPCID.null)
            errObj.data?)).compress
        | .ok () =>
          -- Look up the method in the registry
          match registryWithHandlers.get? req.method with
          | none => pure (Lean.toJson (JsonRPCResponse.error
              JsonRPCErrorCode.methodNotFound
              "Method not found"
              (req.id?.getD JsonRPCID.null))).compress
          | some handler => do
            -- Execute the handler
            let response ← handler req.params? (req.id?.getD JsonRPCID.null)
            pure (Lean.toJson response).compress

  -- Use a different port for testing to avoid conflicts
  let testConfig : ServerConfig := { port := 8089, host := "127.0.0.1", maxBodySize := 1024 }

  -- Start the server in a separate task
  let severStopFlag ← IO.mkRef false
  let serverTask ← IO.asTask (startJsonRpcServer testConfig jsonRpcHandler severStopFlag)

  -- Give the server time to start up
  IO.sleep 500

  -- Now make real HTTP calls to test the server
  try
    -- Test 1: Valid add request - need to serialize parameters properly
    let data1 : Lean.Json := LeanSerial.serialize (5 : Nat)
    let data2 : Lean.Json := LeanSerial.serialize (3 : Nat)
    let param1 := data1.compress
    let param2 := data2.compress
    let addRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"add\",\"params\":[" ++ param1 ++ "," ++ param2 ++ "],\"id\":1}"
    let addResult ← makeJsonHttpRequest testConfig addRequest
    let addSuccess := match addResult with
      | .ok httpResponse =>
        match extractResponseBody httpResponse with
        | .ok body =>
          stringContains body "\"result\":" && stringContains body "\"id\":1"
        | .error _ => false
      | .error _ => false

    -- Test 2: Method not found
    let badMethodRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"nonexistent\",\"id\":2}"
    let badMethodResult ← makeJsonHttpRequest testConfig badMethodRequest
    let badMethodSuccess := match badMethodResult with
      | .ok httpResponse =>
        match extractResponseBody httpResponse with
        | .ok body =>
          stringContains body "Method not found" && stringContains body "\"id\":2"
        | .error _ => false
      | .error _ => false

    -- Test 3: Invalid JSON
    let invalidJsonRequest := "{invalid json"
    let invalidJsonResult ← makeJsonHttpRequest testConfig invalidJsonRequest
    let invalidJsonSuccess := match invalidJsonResult with
      | .ok httpResponse =>
        match extractResponseBody httpResponse with
        | .ok body =>
          stringContains body "Parse error"
        | .error _ => false
      | .error _ => false

    -- Test 4: Echo function with string parameter - serialize the string parameter
    let data : Lean.Json := LeanSerial.serialize "Hello World"
    let echoParam := data.compress
    let echoRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"echo\",\"params\":[" ++ echoParam ++ "],\"id\":3}"
    let echoResult ← makeJsonHttpRequest testConfig echoRequest
    let echoSuccess := match echoResult with
      | .ok httpResponse =>
        match extractResponseBody httpResponse with
        | .ok body =>
          stringContains body "Echo: Hello World" && stringContains body "\"id\":3"
        | .error _ => false
      | .error _ => false

    -- Test 5: GetTime function (no parameters)
    let timeRequest := "{\"jsonrpc\":\"2.0\",\"method\":\"getTime\",\"id\":4}"
    let timeResult ← makeJsonHttpRequest testConfig timeRequest
    let timeSuccess := match timeResult with
      | .ok httpResponse =>
        match extractResponseBody httpResponse with
        | .ok body =>
          stringContains body "2024-07-26T12:00:00Z" && stringContains body "\"id\":4"
        | .error _ => false
      | .error _ => false

    -- Check all test results
    if addSuccess && badMethodSuccess && invalidJsonSuccess && echoSuccess && timeSuccess then
      return assert true "Full server integration with real HTTP calls works"
    else
      let failures := []
        |> (if !addSuccess then (· ++ ["Add function failed"]) else id)
        |> (if !badMethodSuccess then (· ++ ["Method not found handling failed"]) else id)
        |> (if !invalidJsonSuccess then (· ++ ["Invalid JSON handling failed"]) else id)
        |> (if !echoSuccess then (· ++ ["Echo function failed"]) else id)
        |> (if !timeSuccess then (· ++ ["GetTime function failed"]) else id)
      return assert false s!"Integration test failures: {failures}"

  catch e =>
    return assert false s!"HTTP client error: {e}"
  finally
    severStopFlag.set true
    IO.sleep 1200
    try
      let _ := serverTask
    catch _ => pure ()

def runHTTPTests : IO Unit := do
  runTests #[
    ("JSON Response", testJsonResponse),
    ("Error Response", testErrorResponse),
    ("Extract JSON Body Valid", testExtractJsonBodyValid),
    ("Reject Non-POST", testRejectNonPost),
    ("Missing Content-Length", testMissingContentLength),
    ("Empty Request", testEmptyRequest),
    ("Server Config Defaults", testServerConfigDefaults),
    ("Body Length Mismatch", testBodyLengthMismatch),
    ("JSON Handler Integration", testWithJsonHandler),
    ("Handler Error Handling", testHandlerError),
    ("Full Server Integration", testFullServerIntegration)
  ]

end LeanRPC.Tests.HTTP
