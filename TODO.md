
* how to Ctrl-C the server-process ?

* Error handling: transport error-messages to client (only in stdout at the moment)

* typecheck module
see: haskell-tools
also: https://github.com/edsko/ghc-dump-tree/blob/a13f2622dff088b1fa95d30886e010280f63edeb/src/Language/Haskell/GHC/DumpTree.hs#L294


* something useful in https://github.com/edsko/ghc-dump-tree ?


Optimizations
=============
* replace showRichTokenStream by actual source; check for differences !

* cache results of lexing/parsing/usage-links etc. on the server (before sending to the client, write the response in a local file)

* request as "one-source-at-a-time" vs. complete project in one request

* String/ByteString efficiency... use Data.Text ?

* check for spaceleaks (GHC stack-limit technique, see Neil Mitchell, eXchange 2016)
