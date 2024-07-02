### A (non-production-ready) job processing API for Elang and Elixir.
- Erlang version is built using cowboy and digraph.
- Elixir version is built using plug/cowboy, jason and libgraph.


### Endpoints
`POST /process` endpoints receives a job payload in below format:
```json
{
  "tasks": [
    { "name": "task-1", "command": "touch /tmp/file1" },
    { "name": "task-2", "command":"cat /tmp/file1", "requires":["task-3"]}, 
    { "name": "task-3", "command": "echo 'Hello World!' > /tmp/file1", "requires":["task-1"] },
    { "name": "task-4", "command": "rm /tmp/file1", "requires":["task-2","task-3"]}
  ]
}
```
And returns tasks in correct order.
```json
{
  "tasks": [
    { "name": "task-1", "command": "touch /tmp/file1" },
    { "name": "task-3", "command": "echo 'Hello World!' > /tmp/file1" },
    { "name": "task-2", "command":"cat /tmp/file1" }, 
    { "name": "task-4", "command": "rm /tmp/file1" }
  ]
}
```
Optionally, a bash script representation can be returned by passing `response_type`.
```json
{
  "response_type": "bash",
  "tasks": []
}
```
```bash
#!/usr/bin/env bash
touch /tmp/file1
echo "Hello World!" > /tmp/file1
cat /tmp/file1
rm /tmp/file1
```

### Erlang implementation
#### Notes
The endpoint either returns correctly sorted tasks or one of the following errors:
```json
{"error": "invalid-payload"}
{"error": "no-tasks-given"}
{"error": "invalid-tasks-list"}
{"error": "invalid-response-type"}
{"error": "invalid-optional-root-fields"}
{"error": "invalid-task-command-or-name"}
{"error": "invalid-task-requires"}
{"error": "invalid-task-name-in-requires"}
{"error": "circular-dependency"}
```
#### Install
```erl
rebar3 compile
```
#### Run
```erl
rebar3 as dev shell
```
#### Test
```erl
rebar3 eunit
```

### Elixir implementation
#### Notes
Circular dependencies, non-existing or self-referencing task names inside `requires`, and many other malformations in the payload return a generic error message.
#### Install
```elixir
mix deps.get
```
#### Run
```elixir
mix run --no-halt
```
#### Test
```elixir
mix test
```