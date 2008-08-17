#!/usr/bin/env runhaskell
> import Distribution.Simple
> import qualified Data.List as L
> import System.Directory
> import System.Process (runCommand,waitForProcess)


> main = defaultMainWithHooks simpleUserHooks
