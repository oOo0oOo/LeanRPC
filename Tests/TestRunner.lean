import Tests.ProtocolTests

def main : IO Unit := do
  IO.println "=== LeanRPC Tests ==="
  LeanRPC.Tests.Protocol.runProtocolTests
