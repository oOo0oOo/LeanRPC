import LeanRPC.Attribute
import LeanRPC.Registry
import Tests.TestFramework
import Lean.Elab.Command
import LeanSerde

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
def testRPCAdd (a b : Nat) : IO Nat := pure (a + b)

@[rpc]
def testRPCNumCombos (names : List String) (cats : List Nat) : IO Nat := pure (names.length * cats.length)

-- This command will be elaborated at compile time, after the attributes have been processed.
init_RPC

-- Test if the registry was built correctly
def testRegistryInitialization (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry
  let methods := listMethods registry

  let hasAdd := methods.contains "testRPCAdd"
  let hasCombos := methods.contains "testRPCNumCombos"

  if hasAdd && hasCombos then
    return assert true "Registry initialization successful"
  else
    return assert false s!"Registry initialization failed. Methods: {methods}"

-- Test running the handlers
def testRPCAddition (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry

  let param1 := LeanSerde.serialize (33 : Nat)
  let param2 := LeanSerde.serialize (11 : Nat)
  let params := Lean.Json.arr #[param1, param2]

  let addHandler := registry.get? "testRPCAdd"
  match addHandler with
  | some handler => do
      let response ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 1)
      match response.result? with
      | some result => do
        match LeanSerde.deserialize result with
        | .ok (44 : Nat) => return assert true "Addition handler works"
        | .ok val => return assert false s!"Addition handler returned wrong value: {val}"
        | .error err => return assert false s!"Addition handler deserialization failed: {err}"
      | none => return assert false "Addition handler returned no result"
  | none => return assert false "Addition handler not found"

def testRPCCombos (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry

  let serializedNames : Lean.Json := LeanSerde.serialize ["Alice", "Bob", "Charlie"]
  let serializedCats : Lean.Json := LeanSerde.serialize [3, 4, 5]

  let params := Lean.Json.arr #[serializedNames, serializedCats]

  let lengthHandler := registry.get? "testRPCNumCombos"
  match lengthHandler with
  | some handler => do
      let response ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 2)
      match response.result? with
      | some result => do
        match LeanSerde.deserialize result with
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
    ("RPC Addition", testRPCAddition),
    ("RPC Length", testRPCCombos)
  ]

end LeanRPC.Tests.Attribute
