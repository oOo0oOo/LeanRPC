import LeanRPC

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

-- Initialize after the RPC functions are registered
init_RPC

def main : IO Unit := do
  IO.println "Try:\ncurl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\": \"2.0\", \"method\": \"fibonacci\", \"params\": [10], \"id\": 1}' http://127.0.0.1:8080"

  -- Start the RPC server
  let config : ServerConfig := {
    host := "127.0.0.1",
    port := 8080
  }
  startRPCServer config buildRPC  -- buildRPC is created during init_RPC
