import LeanRPC
open LeanRPC.Server

-- Mark a function as an RPC method
@[rpc]
partial def collatzSequence (n : Nat) : IO (Except String (List Nat)) := do
  let rec go n acc :=
    if n == 1 then (1 :: acc).reverse
    else go (if n % 2 == 0 then n / 2 else 3 * n + 1) (n :: acc)
  if n == 0 then pure (.error "Start number must be positive")
  else pure (.ok (go n []))

-- Initialize the function registry
init_RPC

def main : IO Unit := do
  IO.println "Try:\ncurl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"collatzSequence\",\"params\":[50],\"id\":1}' http://localhost:8080"

  -- Start server (buildRPC is created during init_RPC)
  startRPCServer { port := 8080 } buildRPC
