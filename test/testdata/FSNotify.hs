-- comment 1
{-# LANGUAGE CPP #-}

module FSNotify where

#if defined(mingw32_HOST_OS)
forkFinally :: String
forkFinally = ""
#endif