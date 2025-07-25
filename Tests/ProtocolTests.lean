import LeanRPC.Protocol
import Tests.TestFramework
import Lean.Data.Json

namespace LeanRPC.Tests.Protocol

open LeanRPC.Protocol
open LeanRPC.Tests

-- Test JsonRPCID serialization
def testJsonRPCIDSerialization (_ : Unit) : TestResult :=
  let idStr := JsonRPCID.str "test123"
  let idNum := JsonRPCID.num 42
  let idNull := JsonRPCID.null

  let jsonStr := Lean.toJson idStr
  let jsonNum := Lean.toJson idNum
  let jsonNull := Lean.toJson idNull

  match Lean.fromJson? jsonStr, Lean.fromJson? jsonNum, Lean.fromJson? jsonNull with
  | .ok (JsonRPCID.str "test123"), .ok (JsonRPCID.num 42), .ok JsonRPCID.null =>
    assert true "JsonRPCID serialization works"
  | _, _, _ =>
    assert false "JsonRPCID serialization failed"

-- Test JsonRPCRequest creation and validation
def testJsonRPCRequestValidation (_ : Unit) : TestResult :=
  let validReq : JsonRPCRequest := {
    method := "add",
    params? := some (.arr #[.num 1, .num 2]),
    id? := some (JsonRPCID.str "req1")
  }

  let invalidReq : JsonRPCRequest := {
    method := "",
    id? := some (JsonRPCID.str "req2")
  }

  let notificationReq : JsonRPCRequest := {
    method := "notify",
    id? := none
  }

  match validReq.validate, invalidReq.validate, notificationReq.validate with
  | .ok (), .error _, .error _ =>
    assert true "Request validation works correctly"
  | _, _, _ =>
    assert false "Request validation failed"

-- Test JsonRPCResponse creation
def testJsonRPCResponseCreation (_ : Unit) : TestResult :=
  let successResp := JsonRPCResponse.success (.num 42) (JsonRPCID.str "test")
  let errorResp := JsonRPCResponse.error JsonRPCErrorCode.methodNotFound "Method not found" (JsonRPCID.str "test")

  let hasResult := successResp.result?.isSome && successResp.error?.isNone
  let hasError := errorResp.error?.isSome && errorResp.result?.isNone

  if hasResult && hasError then
    assert true "Response creation works"
  else
    assert false "Response creation failed"

-- Test JSON serialization roundtrip
def testJsonRoundtrip (_ : Unit) : TestResult :=
  let originalReq : JsonRPCRequest := {
    method := "multiply",
    params? := some (.arr #[.num 5, .num 6]),
    id? := some (JsonRPCID.str "test123")
  }

  let json := Lean.toJson originalReq
  match Lean.fromJson? json with
  | .ok (parsedReq : JsonRPCRequest) =>
    if originalReq.method == parsedReq.method &&
       originalReq.params? == parsedReq.params? &&
       originalReq.id? == parsedReq.id? then
      assert true "JSON roundtrip successful"
    else
      assert false "JSON roundtrip data mismatch"
  | .error msg =>
    assert false s!"JSON parsing failed: {msg}"

-- Run all protocol tests
def runProtocolTests : IO Unit := do
  runTests [
    ("JsonRPCID Serialization", testJsonRPCIDSerialization),
    ("JsonRPCRequest Validation", testJsonRPCRequestValidation),
    ("JsonRPCResponse Creation", testJsonRPCResponseCreation),
    ("JSON Roundtrip", testJsonRoundtrip)
  ]

end LeanRPC.Tests.Protocol
