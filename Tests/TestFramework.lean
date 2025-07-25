namespace LeanRPC.Tests

inductive TestResult where
  | success : String -> TestResult
  | failure : String -> TestResult
  deriving Inhabited

def TestResult.passed (r : TestResult) : Bool :=
  match r with | .success _ => true | .failure _ => false

def TestResult.message (r : TestResult) : String :=
  match r with | .success msg | .failure msg => msg

def assert (cond : Bool) (msg : String) : TestResult :=
  if cond then .success msg else .failure msg

def runTest (name : String) (test : Unit → TestResult) : IO Unit := do
  let result := test ()
  let mark := if result.passed then "✓" else "✗"
  IO.println s!"{mark} {name}: {result.message}"

def runTests (tests : Array (String × (Unit → IO TestResult))) : IO Unit := do
  let mut successes := 0
  let mut failures := 0

  for (name, testFn) in tests do
    let result ← testFn ()
    let mark := if result.passed then "✓" else "✗"
    IO.println s!"{mark} {name}: {result.message}"
    if result.passed then successes := successes + 1 else failures := failures + 1

  IO.println s!"Summary: {successes} passed, {failures} failed.\n"
  if failures > 0 then throw (IO.userError "Some tests failed.")

end LeanRPC.Tests
