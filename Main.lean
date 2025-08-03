import LeanRPC
import LeanSerde.SnapshotTypes

open LeanRPC.Server

-- Mark functions as RPC methods
@[rpc]
partial def collatzSequence (n : Nat) : IO (Except String (List Nat)) := do
  let rec go n acc :=
    if n == 1 then (1 :: acc).reverse
    else go (if n % 2 == 0 then n / 2 else 3 * n + 1) (n :: acc)
  if n == 0 then pure (.error "Start number must be positive")
  else pure (.ok (go n []))

-- Example for stateful methods: Simple REPL
-- Note: LeanSnapshot requires `supportInterpreter = true` in `lakefile.toml`
initialize replState : IO.Ref (Option LeanSerde.LeanSnapshot) ← IO.mkRef none

@[rpc]
def repl_reset (imports : List String) : IO Bool := do
  replState.set (some (← LeanSerde.LeanSnapshot.create imports))
  pure true

@[rpc]
def repl_command (cmd : String) : IO (List String) := do
  match ← replState.get with
  | none => throw (IO.userError "REPL not initialized")
  | some snapshot => do
    let snapshot' ← snapshot.command cmd
    replState.set (some snapshot')
    snapshot'.goals

@[rpc]
def repl_tactic (tactic : String) : IO (List String) := do
  match ← replState.get with
  | none => throw (IO.userError "REPL not initialized")
  | some snapshot => do
    let snapshot' ← snapshot.tactic tactic
    replState.set (some snapshot')
    snapshot'.goals

-- Initialize the function registry
init_RPC

def main : IO Unit := do
  IO.println "Interactions to try:"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"collatzSequence\",\"params\":[50],\"id\":1}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_reset\",\"params\":[[\"List.cons\", [\"Init\"]]],\"id\":2}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_command\",\"params\":[\"theorem my_theorem : 1 + 1 = 2 := by sorry\"],\"id\":3}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_tactic\",\"params\":[\"simp\"],\"id\":4}' http://localhost:8080\n"

  -- Start server (buildRPC is created during init_RPC)
  startRPCServer { port := 8080 } buildRPC
