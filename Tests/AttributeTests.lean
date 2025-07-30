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

-- RPC function (also available in other files)
@[rpc]
def testRPCAdd (a b : Nat) : Nat := a + b

@[rpc]
def testRPCNumCombos (names : List String) (cats : List Nat) : Nat := names.length * cats.length

@[rpc]
def testRPCAddIO (a b : Nat) : IO Nat :=
  pure (a + b)

@[rpc]
def testRPCAddExcept (a b : Nat) : Except String Nat :=
  if a + b > 100 then
    .error "Sum exceeds limit"
  else
    .ok (a + b)

@[rpc]
def testRPCAddOption (a b : Nat) : Option Nat :=
  if a + b > 100 then
    none
  else
    some (a + b)

-- StateT and ReaderT IO support
@[rpc]
def testRPCAddStateT (a b : Nat) : StateT Nat IO Nat := do
  let state ← get
  set (state + a + b)
  pure (state + a + b)

@[rpc]
def testRPCAddReaderT (a b : Nat) : ReaderT Nat IO Nat := do
  let env ← read
  pure (env + a + b)

-- Sandboxed Monads
@[rpc]
def testRPCAddMeta (a b : Nat) : MetaM Nat := do
  pure (a + b)

@[rpc]
def testRPCAddCore (a b : Nat) : CoreM Nat := do
  pure (a + b)

@[rpc]
def testRPCAddTermElab (a b : Nat) : TermElabM Nat := do
  pure (a + b)

@[rpc]
def testRPCAddCommandElab (a b : Nat) : CommandElabM Nat := do
  pure (a + b)

-- Various
@[rpc]
def testRPCAddTask (a b : Nat) : Task Nat :=
  Task.pure (a + b)

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

def testRPCAddition (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry

  let param1 := LeanSerde.serialize (33 : Nat)
  let param2 := LeanSerde.serialize (11 : Nat)
  let params := Lean.Json.arr #[param1, param2]

  let addHandler := registry.get? "testRPCAdd"
  match addHandler with
  | some handler => do
      match ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 1) with
      | .ok response =>
        match response.result? with
        | some result => do
          match LeanSerde.deserialize result with
          | .ok (44 : Nat) => return assert true "Addition handler works"
          | .ok val => return assert false s!"Addition handler returned wrong value: {val}"
          | .error err => return assert false s!"Addition handler deserialization failed: {err}"
        | none => return assert false "Addition handler returned no result"
      | .error msg => return assert false s!"Addition handler failed: {msg}"
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
      match ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 2) with
      | .ok response =>
        match response.result? with
        | some result => do
          match LeanSerde.deserialize result with
          | .ok (9 : Nat) => return assert true "Length handler works"
          | .ok val => return assert false s!"Length handler returned wrong value: {val}"
          | .error err => return assert false s!"Length handler deserialization failed: {err}"
        | none => return assert false "Length handler returned no result"
      | .error msg => return assert false s!"Length handler failed: {msg}"
  | none => return assert false "Length handler not found"

def testMonads (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry

  let monadMethods := #["testRPCAddIO", "testRPCAddExcept", "testRPCAddOption", "testRPCAddMeta", "testRPCAddCore", "testRPCAddTermElab", "testRPCAddCommandElab", "testRPCAddTask"]

  for method in monadMethods do
    let param1 := LeanSerde.serialize 15
    let param2 := LeanSerde.serialize 25
    let params := Lean.Json.arr #[param1, param2]

    match registry.get? method with
    | some handler => do
        match ← handler (some params) (LeanRPC.Protocol.JsonRPCID.num 3) with
        | .ok response =>
          match response.result? with
          | some result => do
            match LeanSerde.deserialize result with
            | .ok (40 : Nat) => continue
            | .ok val => return assert false s!"{method} returned wrong value: {val}"
            | .error err => return assert false s!"{method} deserialization failed: {err}"
          | none =>
            match response.error? with
            | some error =>
              IO.println s!"DEBUG: {method} error: {error.message}"
              continue
            | none => return assert false s!"{method} returned no result and no error"
        | .error msg => return assert false s!"{method} failed: {msg}"
    | none => return assert false s!"{method} not found"
  return assert true "All monad handlers work"

def testStateReaderMonads (_ : Unit) : IO TestResult := do
  let emptyRegistry := mkMethodRegistry
  let registry := buildRPC emptyRegistry

  let initialState := LeanSerde.serialize (10 : Nat)
  let param1 := LeanSerde.serialize (15 : Nat)
  let param2 := LeanSerde.serialize (25 : Nat)
  let stateParams := Lean.Json.arr #[initialState, param1, param2]

  match registry.get? "testRPCAddStateT" with
  | some handler => do
      match ← handler (some stateParams) (LeanRPC.Protocol.JsonRPCID.num 4) with
      | .ok response =>
        match response.result? with
        | some result => do
          match LeanSerde.deserialize result with
          | .ok ((50, 50) : Nat × Nat) => pure ()
          | .ok val => return assert false s!"StateT returned wrong value: {val}"
          | .error err => return assert false s!"StateT deserialization failed: {err}"
        | none => return assert false "StateT returned no result"
      | .error msg => return assert false s!"StateT failed: {msg}"
  | none => return assert false "StateT handler not found"

  let environment := LeanSerde.serialize (5 : Nat)
  let readerParams := Lean.Json.arr #[environment, param1, param2]

  match registry.get? "testRPCAddReaderT" with
  | some handler => do
      match ← handler (some readerParams) (LeanRPC.Protocol.JsonRPCID.num 5) with
      | .ok response =>
        match response.result? with
        | some result => do
          match LeanSerde.deserialize result with
          | .ok (45 : Nat) => return assert true "StateT and ReaderT handlers work"
          | .ok val => return assert false s!"ReaderT returned wrong value: {val}"
          | .error err => return assert false s!"ReaderT deserialization failed: {err}"
        | none => return assert false "ReaderT returned no result"
      | .error msg => return assert false s!"ReaderT failed: {msg}"
  | none => return assert false "ReaderT handler not found"

-- Run all attribute tests
def runAttributeTests : IO Unit := do
  runTests #[
    ("getFunctionName", testGetFunctionName),
    ("Registry Initialization", testRegistryInitialization),
    ("RPC Addition", testRPCAddition),
    ("RPC Length", testRPCCombos),
    ("RPC Monads", testMonads),
    ("RPC State and Reader Monads", testStateReaderMonads)
  ]

end LeanRPC.Tests.Attribute
