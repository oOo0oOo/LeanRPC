import LeanRPC.Client
import LeanRPC.Server
import LeanRPC.Attribute
import Tests.TestFramework
import Tests.AttributeTests
import LeanSerial

namespace LeanRPC.Tests.Client

open LeanRPC.Tests
open LeanRPC.Client
open LeanRPC.HTTP
open LeanRPC.Protocol

def testClientRoundtrip (_ : Unit) : IO TestResult := do
  let config : ServerConfig := {
    port := 8094,
    host := "127.0.0.1",
    maxBodySize := 1024 * 1024
  }
  let stopServer ← LeanRPC.Server.launchRPCServer config buildRPC
  IO.sleep 500

  try
    let result1 : Except String Nat ← callRPC config "testRPCAdd" [42, 17]
    let success1 ← match result1 with
      | .ok 59 => pure true
      | .ok val => do
        IO.println s!"[FAIL] testRPCAdd: Wrong value. Expected 59, got {val}"
        pure false
      | .error err => do
        IO.println s!"[FAIL] testRPCAdd: RPC call failed. Error: {err}"
        pure false

    let names := LeanSerial.serialize ["Alice", "Bob"]
    let lastNames := LeanSerial.serialize [9, 99, 999]
    let params := [names, lastNames]
    let result2 : Except String Nat ← callRPC config "testRPCNumCombos" params (JsonRPCID.num 9999)
    let success2 ← match result2 with
      | .ok 6 => pure true
      | .ok val => do
        IO.println s!"[FAIL] testRPCNumCombos: Wrong value. Expected 6, got {val}"
        pure false
      | .error err => do
        IO.println s!"[FAIL] testRPCNumCombos: RPC call failed. Error: {err}"
        pure false

    if success1 && success2 then
      return assert true "Client roundtrip tests passed"
    else
      return assert false s!"One or more client tests failed. See logs above."

  catch e =>
    return assert false s!"Test failed with unhandled exception: {e}"
  finally
    stopServer

def runClientTests : IO Unit := do
  runTests #[
    ("Client Roundtrip", testClientRoundtrip)
  ]

end LeanRPC.Tests.Client
