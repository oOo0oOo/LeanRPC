import Tests.ProtocolTests
import Tests.RegistryTests

def main : IO Unit := do
  IO.println "=== LeanRPC Tests ==="
  LeanRPC.Tests.Protocol.runProtocolTests
  LeanRPC.Tests.Registry.runRegistryTests
