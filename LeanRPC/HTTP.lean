import Std.Internal.Async.TCP
import Std.Net.Addr
import Lean.Data.Json
import Std.Time

namespace LeanRPC.HTTP

open Std.Internal.IO.Async.TCP
open Std.Net

def getHTTPStatusMessage (code : Nat) : String :=
  match code with
  | 200 => "OK"
  | 400 => "BAD_REQUEST"
  | 404 => "NOT_FOUND"
  | 405 => "METHOD_NOT_ALLOWED"
  | 411 => "LENGTH_REQUIRED"
  | 413 => "PAYLOAD_TOO_LARGE"
  | 415 => "UNSUPPORTED_MEDIA_TYPE"
  | 500 => "INTERNAL_SERVER_ERROR"
  | _ => "UNKNOWN_STATUS"

-- SERVER
structure ServerConfig where
  port : UInt16 := 8080
  host : String := "127.0.0.1"
  maxBodySize : Nat := 1024 * 1024
  logging: Bool := true
  deriving Inhabited, BEq

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

def jsonResponse (body : String) (status : Nat := 200) : String :=
  let reason := getHTTPStatusMessage status
  let headers := s!"Content-Type: application/json\r\nContent-Length: {body.length}\r\nConnection: close\r\n"
  s!"HTTP/1.1 {status} {reason}\r\n{headers}\r\n{body}"

def errorResponse (message : String) (status : Nat := 400) : String :=
  let errorJson := "{\"error\":\"" ++ message ++ "\"}"
  jsonResponse errorJson status

private def getMethodName (jsonBody : String) : String :=
  match Lean.Json.parse jsonBody with
  | .ok j => (j.getObjVal? "method" >>= (·.getStr?)).toOption.getD "unknown"
  | .error _ => "parse_error"

def extractJsonBody (data : String) : Except (Nat × String) String := do
  let parts := data.splitOn "\r\n\r\n"
  if parts.length < 2 then throw (400, "Missing body")
  let headers := parts.head!
  let body := parts.drop 1 |> String.intercalate "\r\n\r\n"

  let headerLines := headers.splitOn "\r\n"
  if headerLines.isEmpty then throw (400, "Missing request line")

  let requestLine := headerLines.head!
  let requestParts := requestLine.split (· == ' ')
  if requestParts.length < 2 then throw (400, "Malformed request line")
  if requestParts[0]! != "POST" then throw (405, "Only POST requests are supported")
  if requestParts[1]! != "/" then throw (404, "Not found")

  let contentTypeLine? := headerLines.find? (·.toLower.startsWith "content-type:")
  let contentType? := contentTypeLine?.bind (fun line => (line.splitOn ":")[1]?.map (·.trim.toLower))
  if contentType? != some "application/json" then
    throw (415, "Content-Type must be application/json")

  let contentLengthLine? := headerLines.find? (·.toLower.startsWith "content-length:")
  let contentLength? := contentLengthLine?.bind (fun line => (line.splitOn ":")[1]? >>= (·.trim.toNat?))

  match contentLength? with
  | none => throw (411, "Missing or invalid Content-Length header")
  | some contentLength =>
    -- We don't care about the exact Content-Length: Many UTF-8 encodings in Lean code.
    return body.take contentLength

private def logRequest (remoteAddr : String) (logMessage : String) (status : Nat) : IO Unit := do
  let now ← Std.Time.PlainDateTime.now
  let timeStr := (toString now.time).splitOn "." |>.head!
  let statusStr := getHTTPStatusMessage status
  IO.println s!"[{now.date} {timeStr}] {remoteAddr} \"{logMessage}\" {statusStr}"

private def handleConnection (jsonHandler : String → IO String) (client : Socket.Client) (config : ServerConfig) : IO Unit := do
  try
    let remoteAddrObj ← client.getPeerName
    let remoteAddr := s!"{remoteAddrObj.ipAddr}:{remoteAddrObj.port}"
    let mut requestData := ""
    let mut totalBytes := 0

    while totalBytes < config.maxBodySize do
      let dataOpt ← (← client.recv? 4096).block
      match dataOpt with
      | none => break
      | some bytes =>
        requestData := requestData ++ String.fromUTF8! bytes
        totalBytes := totalBytes + bytes.size
        if requestData.contains '\r' && requestData.contains '\n' then
          if (requestData.splitOn "\r\n\r\n").length > 1 then break

    if totalBytes >= config.maxBodySize then
      let response := jsonResponse "{\"error\":\"Request too large\"}" 413
      if config.logging then
        logRequest remoteAddr "Request too large" 413
      discard <| (← client.send response.toUTF8).block
      return

    let (responseBody, status, logMessage) ← match extractJsonBody requestData with
      | .error (code, msg) => pure ("{\"error\":\"" ++ msg ++ "\"}", code, "HTTP Error: " ++ msg)
      | .ok jsonBody => do
        let methodName := getMethodName jsonBody
        try
          let result ← jsonHandler jsonBody
          pure (result, 200, methodName)
        catch e =>
          pure ("{\"error\":\"" ++ e.toString ++ "\"}", 500, methodName ++ " (failed)")

    if config.logging then
      logRequest remoteAddr logMessage status

    let responseStr := jsonResponse responseBody status
    discard <| (← client.send responseStr.toUTF8).block

  catch _ =>
    if config.logging then
      logRequest "unknown" "Server error" 500
    let response := errorResponse "Internal server error" 500
    try discard <| (← client.send response.toUTF8).block catch _ => pure ()

  try discard <| (← client.shutdown).block catch _ => pure ()

def startJsonRPCServer (config : ServerConfig) (jsonHandler : String → IO String) (stopFlag : IO.Ref Bool) : IO Unit := do
  let server ← Socket.Server.mk

  match config.getServerAddress with
  | .error err => throw $ IO.userError err
  | .ok addr => do
    server.bind addr
    server.listen 128
    if config.logging then
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
        discard <| IO.asTask (handleConnection jsonHandler client config)
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
        s!"POST / HTTP/1.1\r\n" ++
        s!"Host: {config.host}:{config.port}\r\n" ++
        s!"Content-Type: application/json\r\n" ++
        s!"Content-Length: {jsonString.length}\r\n" ++
        s!"Connection: close\r\n\r\n" ++
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
