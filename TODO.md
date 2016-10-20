
Client
======

* fold over interleaved tokens/source to render styled Html

Server
======

* WIP interleave all tokens
  with positions ?

* provide project structure (stack.yaml, cabal) to client (directory-tree)

* WIP parse project structure (stack.yaml, cabal)
  see https://github.com/haskell-tools/haskell-tools/blob/51c41a4727bc317c762e4e9ddc589f4d9f35bf9d/src/cli/Language/Haskell/Tools/Refactor/CLI.hs#L45
  Main.hs missing

* Error handling: transport error-messages to client (only in stdout at the moment)

* typecheck module
  see: haskell-tools
  see also: https://github.com/edsko/ghc-dump-tree/blob/a13f2622dff088b1fa95d30886e010280f63edeb/src/Language/Haskell/GHC/DumpTree.hs#L294

* generally: ? anything useful in https://github.com/edsko/ghc-dump-tree


Optimizations
=============

* WIP use results from Performance*Test in the server
  TODO: measure space/heap usage in Performance*Test
  TODO: PerformanceSequenceOfCharTest

* cache results of lexing/parsing/usage-links etc. on the server, last-modified optimization etc.

* request as "one-source-at-a-time" vs. complete project in one request

* Protolude et al. ?

* check for spaceleaks (GHC stack-limit technique, see Neil Mitchell, eXchange 2016)
