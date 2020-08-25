# Miking-LSP
A Miking Language Server Protocol ([LSP](https://langserver.org/)) implementation

The goals of the Miking-LSP project are to:

* Implement the [LSP](https://langserver.org/) protocol in MCore.

* Implement an LSP language server for Miking in MCore.

* Provide IDE features for both the Miking Core language itself and user-created
  DSLs using the server.

## Getting started

To get started, follow the instructions at the main
[Miking](https://github.com/miking-lang/miking/) repo to install Miking along
with its standard library.

More instructions to come.

## Current status

Currently, this repository contains the following:

- An implementation of the [JSON-RPC](https://www.jsonrpc.org/specification),
  which the LSP protocol is built upon, in [json-rpc.mc](./src/lsp/json-rpc.mc).

- An architecture for an LSP server, in [lsp.mc](./src/lsp/lsp.mc). In the file,
  a more detailed description can be found.

- A dummy server, at [main.mc](./src/mcore-lsp-server/main.mc). Currently, all
  the server does is read messages from `stdin`, parse them as JSON-RPC and
  print them out again. To try it, run for instance:
  ```
  cat test/msg2.test | mi src/mcore-lsp-server/main.mc
  ```
  which should output
  ```
  {"jsonrpc": "2.0", "method": "shutdown", "params": {}, "id": 3}

  FILE "/path/to/miking-lsp/src/lsp/lsp.mc" 55:7-55:38 ERROR: Unexpected end of input
  ```
  The error is expected as the server should normally keep receiving input until
  a shutdown message is received.

Also, a few example LSP messages can be found in the [test](./test) folder.

What is missing is the main part of the protocol's data structures (data types
describing the different kinds of requests, notifications and parameters) and
a proper server implementation for the Miking system.

The [OCaml LSP](https://github.com/ocaml/ocaml-lsp) codebase has been used as a
reference for this project.
