import Lean.Data.Json

namespace LeanRPC.Protocol

inductive JsonRPCID where
  | str (s : String)
  | num (n : Nat)
  | null
  deriving BEq, Inhabited

instance : Lean.ToJson JsonRPCID where
  toJson
    | .str s => .str s
    | .num n => .num n
    | .null => .null

instance : Lean.FromJson JsonRPCID where
  fromJson? j :=
    (JsonRPCID.str <$> Lean.fromJson? j) <|>
    (JsonRPCID.num <$> Lean.fromJson? j) <|>
    (do if j.isNull then pure JsonRPCID.null else throw "expected null")

structure JsonRPCRequest where
  jsonrpc : String := "2.0"
  method : String
  params? : Option Lean.Json := none
  id? : Option JsonRPCID := none
  deriving Lean.FromJson, Lean.ToJson

structure JsonRPCErrorObject where
  code : Int
  message : String
  data? : Option Lean.Json := none
  deriving Lean.FromJson, Lean.ToJson, BEq

structure JsonRPCResponse where
  jsonrpc : String := "2.0"
  result? : Option Lean.Json := none
  error? : Option JsonRPCErrorObject := none
  id : JsonRPCID
  deriving Lean.FromJson, Lean.ToJson, BEq

namespace JsonRPCErrorCode
  def parseError : Int := -32700
  def invalidRequest : Int := -32600
  def methodNotFound : Int := -32601
  def invalidParams : Int := -32602
  def internalError : Int := -32603
end JsonRPCErrorCode

def JsonRPCResponse.success (result : Lean.Json) (id : JsonRPCID) : JsonRPCResponse :=
  { result? := some result, id := id }

def JsonRPCResponse.error (code : Int) (message : String) (id : JsonRPCID) (data? : Option Lean.Json := none) : JsonRPCResponse :=
  { error? := some { code := code, message := message, data? := data? }, id := id }

def JsonRPCRequest.validate (req : JsonRPCRequest) : Except JsonRPCErrorObject Unit := do
  if req.id?.isNone then
    .error { code := JsonRPCErrorCode.invalidRequest, message := "Notifications are not supported" }
  else if req.jsonrpc != "2.0" then
    .error { code := JsonRPCErrorCode.invalidRequest, message := "Invalid JSON-RPC version" }
  else if req.method.isEmpty then
    .error { code := JsonRPCErrorCode.invalidRequest, message := "Method name cannot be empty" }
  else
    .ok ()

end LeanRPC.Protocol
