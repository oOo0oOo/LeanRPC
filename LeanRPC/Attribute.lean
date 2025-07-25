import Lean
import Lean.Meta
import LeanRPC.Registry

namespace LeanRPC.Attribute

open Lean Elab Command Meta

def parseRpcArgs (_declName : Name) (_stx : Syntax) : AttrM Unit := do
  return ()

def getFunctionName (declName : Name) : String :=
  declName.getString!

initialize rpcFunctions : IO.Ref (Array Name) ← IO.mkRef #[]

/-- The @[rpc] attribute implementation -/
def registerRpcFunction (declName : Name) (_config : Unit) : AttrM Unit := do
  let rpcFuns ← liftM (rpcFunctions.get : IO _)
  liftM (rpcFunctions.set (rpcFuns.push declName) : IO _)

initialize rpcAttribute : ParametricAttribute Unit ←
  registerParametricAttribute {
    name := `rpc
    descr := "Mark a function as available for RPC calls"
    getParam := parseRpcArgs
    afterSet := registerRpcFunction
  }

/-- Build the RPC method registry from registered functions -/
elab "initialize_rpc_handlers" : command => do
  let rpcFuns ← liftTermElabM (rpcFunctions.get : IO _)

  let bodyExpr ← if rpcFuns.isEmpty then
    `(baseRegistry)
  else do
    let mut registryExpr ← `(baseRegistry)
    for declName in rpcFuns do
      let methodName := getFunctionName declName
      let funcIdent := mkIdent declName
      let methodNameLit := Syntax.mkStrLit methodName
      registryExpr ← `(LeanRPC.Registry.registerFunction $registryExpr $methodNameLit $funcIdent)
    pure registryExpr

  let declName := `buildRpcRegistry
  let levelParams := []

  liftTermElabM do
    let type ← `(LeanRPC.Registry.MethodRegistry → LeanRPC.Registry.MethodRegistry)
    let value ← `(fun baseRegistry => $bodyExpr)

    let typeExpr ← Term.elabType type
    let valueExpr ← Term.elabTermEnsuringType value typeExpr

    let decl := Declaration.defnDecl {
      name := declName
      levelParams := levelParams
      type := typeExpr
      value := valueExpr
      hints := ReducibilityHints.regular 1
      safety := DefinitionSafety.safe
    }

    addAndCompile decl

end LeanRPC.Attribute
