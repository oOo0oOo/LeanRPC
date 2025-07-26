import LeanRPC

open Lean Elab Command
open LeanRPC.HTTP
open LeanRPC.Server

-- Make functions available for RPC via the @[rpc] attribute
@[rpc]
def fibonacci (n : Nat) : IO Nat := do
  let rec fib : Nat → Nat
    | 0 => 0
    | 1 => 1
    | n + 2 => fib n + fib (n + 1)
  pure (fib n)

-- Compile time initialization of RPC handlers
init_RPC

def main : IO Unit := do
  -- Start the RPC server
  let config : ServerConfig := {
    host := "127.0.0.1",
    port := 8080
  }
  let _stopServer ← startRPCServer config buildRPC  -- buildRPC is created during init_RPC

  IO.println "Try:\ncurl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\": \"2.0\", \"method\": \"fibonacci\", \"params\": [{\"root\": 10, \"objects\": []}], \"id\": 1}' http://127.0.0.1:8080"

  -- Stop the server by calling:
  -- _stopServer
