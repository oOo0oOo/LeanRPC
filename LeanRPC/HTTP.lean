import Std.Internal.Async.TCP
import Std.Net.Addr
import Lean.Data.Json

namespace LeanRPC.HTTP

open Std.Internal.IO.Async.TCP
open Std.Net

structure ServerConfig where
  port : Nat := 8080
  host : String := "127.0.0.1"
  maxBodySize : Nat := 1024 * 1024
  deriving BEq

namespace ServerConfig
def getServerAddress (config : ServerConfig) : Except String SocketAddress := do
  let octets := match config.host.split (· == '.') with
    | [o1, o2, o3, o4] =>
      let parseOctet (s : String) : Option UInt8 := s.toNat?.bind (fun n => if n <= 255 then some n.toUInt8 else none)
      match parseOctet o1, parseOctet o2, parseOctet o3, parseOctet o4 with
      | some a, some b, some c, some d => some ⟨#[a, b, c, d], by rfl⟩
      | _, _, _, _ => none
    | _ => none

  match octets with
  | none => throw s!"Invalid host address: {config.host}"
  | some addr_octets =>
    return SocketAddress.v4 { addr := IPv4Addr.mk addr_octets, port := config.port }
end ServerConfig

-- SERVER
def jsonResponse (body : String) (status : Nat := 200) : String :=
  let headers := s!"Content-Type: application/json\r\nContent-Length: {body.length}\r\nConnection: close\r\n"
  s!"HTTP/1.1 {status} OK\r\n{headers}\r\n{body}"

def errorResponse (message : String) (status : Nat := 400) : String :=
  let errorJson := "{\"error\":\"" ++ message ++ "\"}"
  jsonResponse errorJson status

def extractJsonBody (data : String) : Except String String := do
  let parts := data.splitOn "\r\n\r\n"
  if parts.length < 2 then throw "Invalid HTTP request: missing body"
  let headers := parts.head!
  let body := parts.drop 1 |> String.intercalate "\r\n\r\n"

  let headerLines := headers.splitOn "\r\n"
  if headerLines.isEmpty then throw "Invalid HTTP request: missing request line"

  let requestLine := headerLines.head!
  if !requestLine.startsWith "POST" then throw "Only POST requests are supported"

  let contentLengthLine? := headerLines.find? (·.toLower.startsWith "content-length:")
  let contentLength? := contentLengthLine?.bind (fun line => (line.splitOn ":")[1]? >>= (·.trim.toNat?))

  match contentLength? with
  | none => throw "Missing or invalid Content-Length header"
  | some contentLength =>
    if body.length < contentLength then
      throw s!"Incomplete body: expected {contentLength} bytes, got {body.length}"
    return body.take contentLength

def handleConnection (jsonHandler : String → IO String) (client : Socket.Client) (maxBodySize : Nat) : IO Unit := do
  try
    let mut requestData := ""
    let mut totalBytes := 0

    while totalBytes < maxBodySize do
      let dataOpt ← (← client.recv? 4096).block
      match dataOpt with
      | none => break
      | some bytes =>
        requestData := requestData ++ String.fromUTF8! bytes
        totalBytes := totalBytes + bytes.size
        if requestData.contains '\r' && requestData.contains '\n' then
          if (requestData.splitOn "\r\n\r\n").length > 1 then break

    if totalBytes >= maxBodySize then
      let response := errorResponse "Request too large" 413
      discard <| (← client.send response.toUTF8).block
      return

    let responseStr ← match extractJsonBody requestData with
      | .error err => pure (errorResponse err)
      | .ok jsonBody => do
        try
          let result ← jsonHandler jsonBody
          pure (jsonResponse result)
        catch e =>
          pure (errorResponse e.toString 500)

    discard <| (← client.send responseStr.toUTF8).block

  catch _ =>
    let response := errorResponse "Internal server error" 500
    try discard <| (← client.send response.toUTF8).block catch _ => pure ()

  try discard <| (← client.shutdown).block catch _ => pure ()

def startJsonRpcServer (config : ServerConfig) (jsonHandler : String → IO String) (stopFlag : IO.Ref Bool) : IO Unit := do
  let server ← Socket.Server.mk

  match config.getServerAddress with
  | .error err => throw $ IO.userError err
  | .ok addr => do
    server.bind addr
    server.listen 128
    IO.println s!"JSON-RPC server listening on {config.host}:{config.port}"

    -- Shutdown monitor
    discard <| IO.asTask (do
      while !(← stopFlag.get) do IO.sleep 1000
      try
        let client ← Socket.Client.mk
        (← client.connect addr).block
        (← client.shutdown).block
      catch _ => pure ())

    while !(← stopFlag.get) do
      try
        let client ← (← server.accept).block
        if ← stopFlag.get then
          try (← client.shutdown).block catch _ => pure ()
          break
        discard <| IO.asTask (handleConnection jsonHandler client config.maxBodySize)
      catch e =>
        if ← stopFlag.get then break
        IO.eprintln s!"Accept error: {e}"
        IO.sleep 100

-- CLIENT
def makeJsonHttpRequest (config: ServerConfig) (jsonString : String) : IO (Except String String) := do
  try
    let client ← Socket.Client.mk

    match config.getServerAddress with
    | .error err => return .error err
    | .ok addr => do

      (← client.connect addr).block

      let httpRequest :=
        s!"POST /rpc HTTP/1.1\r\n" ++
        s!"Host: {config.host}:{config.port}\r\n" ++
        s!"Content-Type: application/json\r\n" ++
        s!"Content-Length: {jsonString.length}\r\n" ++
        s!"Connection: close\r\n" ++
        s!"\r\n" ++
        jsonString

      (← client.send httpRequest.toUTF8).block

      let mut responseData := ""
      let mut totalBytes := 0

      while totalBytes < config.maxBodySize do
        let dataOpt ← (← client.recv? 1024).block
        match dataOpt with
        | none => break
        | some bytes =>
          responseData := responseData ++ String.fromUTF8! bytes
          totalBytes := totalBytes + bytes.size
          if (responseData.splitOn "\r\n\r\n").length > 1 then break

      try (← client.shutdown).block catch _ => pure ()
      return .ok responseData
  catch e =>
    return .error e.toString

end LeanRPC.HTTP
