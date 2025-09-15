<h1 align="center">
  LeanRPC
</h1>

<h3 align="center">Serve Lean 4 functions as RPC methods over HTTP</h3>

<p align="center">
  <a href="https://github.com/leanprover/lean4/releases/tag/v4.23.0">
    <img src="https://img.shields.io/badge/Lean-v4.23.0-blue" alt="Lean version" />
  </a>
  <a href="">
    <img src="https://img.shields.io/github/last-commit/oOo0oOo/LeanRPC" alt="last update" />
  </a>
  <a href="https://github.com/oOo0oOo/LeanRPC/blob/main/LICENSE">
    <img src="https://img.shields.io/github/license/oOo0oOo/LeanRPC.svg" alt="license" />
  </a>
</p>

## Key Features

- **Attribute-Based RPC**: Mark functions with `@[rpc]` to expose as JSON-RPC endpoints via HTTP server
- **Type-Safe Serialization**: Automatic JSON handling with compile-time type checking
- **Multi-Monad Support**: `IO`, `Except`, `StateT`, `ReaderT`, and Lean elaboration contexts

## Usage

### Installation

Add `LeanRPC` as a dependency to your `lakefile.toml` and run `lake update`:

```toml
[[require]]
name = "LeanRPC"
git = "https://github.com/oOo0oOo/LeanRPC.git"
rev = "main"
```

### Quick Start

```lean
import LeanRPC
import LeanSerde.SnapshotTypes

open LeanRPC.Server

-- Mark functions as RPC methods
@[rpc]
partial def collatzSequence (n : Nat) : IO (Except String (List Nat)) := do
  let rec go n acc :=
    if n == 1 then (1 :: acc).reverse
    else go (if n % 2 == 0 then n / 2 else 3 * n + 1) (n :: acc)
  if n == 0 then pure (.error "Start number must be positive")
  else pure (.ok (go n []))

-- Example for stateful methods: Simple REPL
-- Note: LeanSnapshot requires `supportInterpreter = true` in `lakefile.toml`
initialize replState : IO.Ref (Option LeanSerde.LeanSnapshot) ← IO.mkRef none

@[rpc]
def repl_reset (imports : List String) : IO Bool := do
  replState.set (some (← LeanSerde.LeanSnapshot.create imports))
  pure true

@[rpc]
def repl_command (cmd : String) : IO (List String) := do
  match ← replState.get with
  | none => throw (IO.userError "REPL not initialized")
  | some snapshot => do
    let snapshot' ← snapshot.command cmd
    replState.set (some snapshot')
    snapshot'.goals

@[rpc]
def repl_tactic (tactic : String) : IO (List String) := do
  match ← replState.get with
  | none => throw (IO.userError "REPL not initialized")
  | some snapshot => do
    let snapshot' ← snapshot.tactic tactic
    replState.set (some snapshot')
    snapshot'.goals

-- Initialize the function registry
init_RPC

def main : IO Unit := do
  IO.println "Interactions to try:"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"collatzSequence\",\"params\":[50],\"id\":1}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_reset\",\"params\":[[\"List.cons\", [\"Init\"]]],\"id\":2}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_command\",\"params\":[\"theorem my_theorem : 1 + 1 = 2 := by sorry\"],\"id\":3}' http://localhost:8080\n"
  IO.println "curl -X POST -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"repl_tactic\",\"params\":[\"simp\"],\"id\":4}' http://localhost:8080\n"

  -- Start server (buildRPC is created during init_RPC)
  startRPCServer { port := 8080 } buildRPC
```

## Guide

### Defining RPC Functions

Functions are exposed as RPC methods using the `@[rpc]` attribute. Supported signatures: `α₁ → α₂ → ... → αₙ → M β` where all types have `LeanSerde.Serializable` instances.

**Supported monads `M`**:
- Pure (no monad): `β`
- Basic: `IO β`, `Except ε β`, `Option β`
- Transformers: `StateT σ IO β`, `ReaderT ρ IO β`
- Sandboxed: `CoreM β`, `MetaM β`, `TermElabM β`, `CommandElabM β`
- Utilities: `Task β`

Note: Sandboxed monads execute in isolated environments without access to the current Lean context. Limited and relatively slow.

**Examples**: `String → IO String`, `Nat → List α → CoreM Bool`, `StateT Nat IO String`

### Server Configuration

The server can be configured by passing a `ServerConfig` record to `startRPCServer`:

```lean
let config : ServerConfig := {
  port : Nat := 8080
  host : String := "127.0.0.1"  -- Currently only supports IPv4
  maxBodySize : Nat := 1024 * 1024
  logging: Bool := true
}
```

### Data Serialization

LeanRPC uses [LeanSerde](https://github.com/oOo0oOo/LeanSerde) for JSON serialization. Many standard types are supported by default. For custom types you can derive `LeanSerde.Serializable` or implement the `LeanSerde.Serializable` interface manually. See the [LeanSerde documentation](https://github.com/oOo0oOo/LeanSerde#supported-types) for details.

### Built-in Methods

- **list_methods**: Returns a list of all registered RPC methods.

## Client Examples

Once the server is running, you can send JSON-RPC 2.0 requests.

### Lean

```lean
import LeanRPC.Client
open LeanRPC.Client

def main : IO Unit := do
  let result : Except String (List Nat) ← callRPC {} "collatzSequence" [50]
```

### curl

```sh
curl -X POST -H 'Content-Type: application/json' \
  -d '{"jsonrpc": "2.0", "method": "collatzSequence", "params": [50], "id": 1}' \
  http://127.0.0.1:8080/
```

### Rust

```rust
use serde_json::json;

fn main() {
    let response = reqwest::blocking::Client::new()
        .post("http://127.0.0.1:8080")
        .json(&json!({"jsonrpc": "2.0", "method": "collatzSequence", "params": [50], "id": 1}))
        .send().unwrap();
    println!("{}", response.json::<serde_json::Value>().unwrap()["result"]);
}
```

### Python

```python
import requests

response = requests.post(
  "http://127.0.0.1:8080",
  headers={"Content-Type": "application/json"},
  json={"jsonrpc": "2.0", "method": "collatzSequence", "params": [50], "id": 1}
)
```

## Development

### Running Tests

```sh
lake test
```

## Security Considerations

LeanRPC provides no built-in authentication or support for HTTPS. For production, secure endpoints by placing the server behind an authenticating reverse proxy, such as [Nginx](https://www.nginx.com/) or [Caddy](https://caddyserver.com/).

## License

MIT