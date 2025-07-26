import Tests.ProtocolTests
import Tests.RegistryTests
import Tests.AttributeTests
import Tests.HTTPTests

def main : IO Unit := do
  IO.println "=== LeanRPC Tests ===\n"
  LeanRPC.Tests.Protocol.runProtocolTests
  LeanRPC.Tests.Registry.runRegistryTests
  LeanRPC.Tests.Attribute.runAttributeTests
  LeanRPC.Tests.HTTP.runHTTPTests
