import Lean
import Lean.Meta
import LeanRPC.Registry

namespace LeanRPC.Attribute

open Lean Elab Command Meta

def parseRPCArgs (_declName : Name) (_stx : Syntax) : AttrM Unit := do
  return ()

def getFunctionName (declName : Name) : String :=
  declName.getString!

initialize rpcFunctions : IO.Ref (Array Name) ← IO.mkRef #[]

/-- The @[rpc] attribute implementation -/
def registerRPCFunction (declName : Name) (_config : Unit) : AttrM Unit := do
  let rpcFuns ← liftM (rpcFunctions.get : IO _)
  liftM (rpcFunctions.set (rpcFuns.push declName) : IO _)

initialize rpcAttribute : ParametricAttribute Unit ←
  registerParametricAttribute {
    name := `rpc
    descr := "Mark a function as available for RPC calls"
    getParam := parseRPCArgs
    afterSet := registerRPCFunction
  }

/-- Build the RPC registry from functions marked with @[rpc] attribute -/
elab "init_RPC" : command => do
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

  let declName := `buildRPC
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
