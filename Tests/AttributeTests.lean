import LeanRPC.Attribute
import LeanRPC.Registry
import Tests.TestFramework
import Lean.Elab.Command
import LeanSerial

namespace LeanRPC.Tests.Attribute

open Lean Elab Command
open LeanRPC.Attribute
open LeanRPC.Tests
open LeanRPC.Registry

-- Test getFunctionName logic
def testGetFunctionName (_ : Unit) : IO TestResult := do
  let name1 := getFunctionName `myFunc
  if name1 != "myFunc" then
    return assert false "testGetFunctionName: Function name extraction failed"

  let name2 := getFunctionName `some.nested.functionName
  if name2 != "functionName" then
    return assert false "testGetFunctionName: Nested function name extraction failed"

  return assert true "testGetFunctionName passed"

-- Functions to be registered by the attribute
@[rpc]
def testRpcAdd (a b : Nat) : IO Nat := pure (a + b)

@[rpc]
def testRpcNumCombos (names : List String) (lastNames : List String) : IO Nat := pure (names.length * lastNames.length)

-- This command will be elaborated at compile time, after the attributes have been processed.
initialize_rpc_handlers

-- Test if the registry was built correctly
def testRegistryInitialization (_ : Unit) : IO TestResult := do
  let emptyRegistry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let registry := buildRpcRegistry emptyRegistry
  let methods := rpc_listMethods registry

  let hasAdd := methods.contains "testRpcAdd"
  let hasCombos := methods.contains "testRpcNumCombos"

  if hasAdd && hasCombos then
    return assert true "Registry initialization successful"
  else
    return assert false s!"Registry initialization failed. Methods: {methods}"

-- Test running the handlers
def testRpcAddition (_ : Unit) : IO TestResult := do
  let emptyRegistry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let registry := buildRpcRegistry emptyRegistry

  let param1 := LeanSerial.serialize (33 : Nat)
  let param2 := LeanSerial.serialize (11 : Nat)
  let params := Lean.Json.arr #[param1, param2]

  let addHandler := registry.get? "testRpcAdd"
  match addHandler with
  | some handler => do
      let response ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 1)
      match response.result? with
      | some result => do
        match LeanSerial.deserialize result with
        | .ok (44 : Nat) => return assert true "Addition handler works"
        | .ok val => return assert false s!"Addition handler returned wrong value: {val}"
        | .error err => return assert false s!"Addition handler deserialization failed: {err}"
      | none => return assert false "Addition handler returned no result"
  | none => return assert false "Addition handler not found"

def testRpcCombos (_ : Unit) : IO TestResult := do
  let emptyRegistry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let registry := buildRpcRegistry emptyRegistry

  let serializedNames : Lean.Json := LeanSerial.serialize ["Alice", "Bob", "Charlie"]
  let serializedLastNames : Lean.Json := LeanSerial.serialize ["Smith", "Johnson", "Williams"]

  let params := Lean.Json.arr #[serializedNames, serializedLastNames]

  let lengthHandler := registry.get? "testRpcNumCombos"
  match lengthHandler with
  | some handler => do
      let response ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 2)
      match response.result? with
      | some result => do
        match LeanSerial.deserialize result with
        | .ok (9 : Nat) => return assert true "Length handler works"
        | .ok val => return assert false s!"Length handler returned wrong value: {val}"
        | .error err => return assert false s!"Length handler deserialization failed: {err}"
      | none => return assert false "Length handler returned no result"
  | none => return assert false "Length handler not found"

-- Run all attribute tests
def runAttributeTests : IO Unit := do
  runTests #[
    ("getFunctionName", testGetFunctionName),
    ("Registry Initialization", testRegistryInitialization),
    ("RPC Addition", testRpcAddition),
    ("RPC Length", testRpcCombos)
  ]

end LeanRPC.Tests.Attribute
