namespace LeanRPC.Tests

structure TestResult where
  name : String
  passed : Bool
  message : String

def assert (condition : Bool) (message : String := "Assertion failed") : TestResult :=
  { name := "", passed := condition, message := if condition then "OK" else message }

def assertEquals {α : Type} [BEq α] [ToString α] (expected actual : α) (name : String := "") : TestResult :=
  let passed := expected == actual
  let message := if passed then "OK" else s!"Expected {expected}, got {actual}"
  { name := name, passed := passed, message := message }

def runTest (name : String) (test : Unit → TestResult) : IO Unit := do
  let result := test ()
  let status := if result.passed then "✓" else "✗"
  IO.println s!"{status} {name}: {result.message}"

def runTests (tests : List (String × (Unit → TestResult))) : IO Unit := do
  let mut passed := 0
  let mut total := 0
  for (name, test) in tests do
    let result := test ()
    let status := if result.passed then "✓" else "✗"
    IO.println s!"  {status} {name}: {result.message}"
    if result.passed then passed := passed + 1
    total := total + 1
  IO.println s!"Results: {passed}/{total} tests passed\n"

end LeanRPC.Tests
