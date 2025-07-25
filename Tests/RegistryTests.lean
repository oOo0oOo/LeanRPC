import LeanRPC.Registry
import Tests.TestFramework
import Lean.Data.Json
import LeanSerial

namespace LeanRPC.Tests.Registry

open LeanRPC.Registry
open LeanRPC.Protocol
open LeanRPC.Tests

-- Helper to run IO tests within the existing framework
def runIOTest (name : String) (test : Unit → IO TestResult) : IO Unit := do
  let result ← test ()
  let status := if result.passed then "✓" else "✗"
  IO.println s!"  {status} {name}: {result.message}"

-- Test basic method registration and execution
def testMethodRegistration (_ : Unit) : IO TestResult := do
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let handler : MethodHandler := fun _ id => pure (JsonRPCResponse.success (.str "test") id)
  let updatedRegistry := registerMethod registry "testMethod" handler

  if updatedRegistry.contains "testMethod" then
    match updatedRegistry.get? "testMethod" with
    | some h => do
      let response ← h none (JsonRPCID.str "test")
      match response.result? with
      | some (.str "test") => return assert true "Method registration and execution works"
      | _ => return assert false "Method execution returned wrong result"
    | none => return assert false "Method not found after registration"
  else
    return assert false "Method registration failed"

-- Test function registration with simple function and execution
def testFunctionRegistration (_ : Unit) : IO TestResult := do
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let addFunc : IO Nat := pure 42
  let updatedRegistry := registerFunction registry "getNumber" addFunc

  if updatedRegistry.contains "getNumber" then
    match updatedRegistry.get? "getNumber" with
    | some h => do
      let response ← h none (JsonRPCID.str "test")
      match response.result? with
      | some result => do
        match LeanSerial.deserialize result with
        | .ok (42 : Nat) => return assert true "Function registration and execution works"
        | .ok val => return assert false s!"Function execution returned wrong value: {val}"
        | .error err => return assert false s!"Function execution deserialization failed: {err}"
      | none => return assert false "Function execution returned no result"
    | none => return assert false "Function not found after registration"
  else
    return assert false "Function registration failed"

-- Test complex multi-argument function registration and execution
def testMultiArgFunction (_ : Unit) : IO TestResult := do
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let complexFunc : Nat → Nat → Nat → Nat → Nat → IO Nat :=
    fun a b c d e => pure (a + b + c + d + e)
  let updatedRegistry := registerFunction registry "addFive" complexFunc

  match updatedRegistry.get? "addFive" with
  | some h => do
    let p1: Lean.Json := LeanSerial.serialize (1 : Nat)
    let p2: Lean.Json := LeanSerial.serialize (2 : Nat)
    let p3: Lean.Json := LeanSerial.serialize (3 : Nat)
    let p4: Lean.Json := LeanSerial.serialize (4 : Nat)
    let p5: Lean.Json := LeanSerial.serialize (5 : Nat)
    let params := Lean.Json.arr #[ p1, p2, p3, p4, p5 ]
    let response ← h (some params) (JsonRPCID.str "test")
    match response.result? with
    | some result => do
      match LeanSerial.deserialize result with
      | .ok (15 : Nat) => return assert true "Multi-argument function execution works"
      | .ok val => return assert false s!"Multi-argument function returned wrong value: {val}"
      | .error err => return assert false s!"Multi-argument function deserialization failed: {err}"
    | none => return assert false "Multi-argument function returned no result"
  | none =>
    return assert false "Multi-argument function not found in registry"

-- Test function with complex serialized argument and execution
def testComplexSerializedArg (_ : Unit) : IO TestResult := do
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let nameListFunc : List Lean.Name → IO Nat :=
    fun names => pure names.length
  let updatedRegistry := registerFunction registry "countNames" nameListFunc

  match updatedRegistry.get? "countNames" with
  | some h => do
    let names := [Lean.Name.mkSimple "foo", Lean.Name.mkSimple "bar", Lean.Name.mkSimple "baz"]
    let serializedNames: Lean.Json := LeanSerial.serialize names
    let params := Lean.Json.arr #[ serializedNames ]
    let response ← h (some params) (JsonRPCID.str "test")
    match response.result? with
    | some result => do
      match LeanSerial.deserialize result with
      | .ok (3 : Nat) => return assert true "Complex serialized argument execution works"
      | .ok val => return assert false s!"Complex argument function returned wrong value: {val}"
      | .error err => return assert false s!"Complex argument function deserialization failed: {err}"
    | none => return assert false "Complex argument function returned no result"
  | none =>
    return assert false "Function with complex serialized argument not found in registry"

-- Test handler execution with simple IO function
def testHandlerExecution (_ : Unit) : IO TestResult := do
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let simpleFunc : IO Nat := pure 42
  let updatedRegistry := registerFunction registry "getNumber" simpleFunc

  match updatedRegistry.get? "getNumber" with
  | some h => do
    let response ← h none (JsonRPCID.str "test")
    match response.result? with
    | some result => do
      match LeanSerial.deserialize result with
      | .ok (42 : Nat) => return assert true "Handler execution works"
      | .ok val => return assert false s!"Handler execution returned wrong value: {val}"
      | .error err => return assert false s!"Handler execution deserialization failed: {err}"
    | none => return assert false "Handler execution returned no result"
  | none => return assert false "Handler not found in registry"

-- Test list methods functionality (pure test)
def testListMethods (_ : Unit) : TestResult :=
  let registry : MethodRegistry := Std.HashMap.emptyWithCapacity 16
  let handler : MethodHandler := fun _ id => pure (JsonRPCResponse.success (.str "test") id)
  let registry1 := registerMethod registry "method1" handler
  let registry2 := registerMethod registry1 "method2" handler

  let methods := rpc_listMethods registry2
  if methods.contains "method1" && methods.contains "method2" && methods.length == 2 then
    assert true "List methods works correctly"
  else
    assert false "List methods failed"

-- Run all registry tests
def runRegistryTests : IO Unit := do
  runTest "List Methods" testListMethods
  runIOTest "Method Registration" testMethodRegistration
  runIOTest "Function Registration" testFunctionRegistration
  runIOTest "Multi-Argument Function" testMultiArgFunction
  runIOTest "Handler Execution" testHandlerExecution
  runIOTest "Serialized Argument" testComplexSerializedArg

end LeanRPC.Tests.Registry
