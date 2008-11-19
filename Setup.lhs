#!/usr/bin/env runhaskell
> import Distribution.Simple
> import System.Directory (setCurrentDirectory)
> import System.Process (runCommand,waitForProcess)


> main = defaultMainWithHooks simpleUserHooks { runTests = tests }

> tests _ _ _ _ = do
>   setCurrentDirectory "src"
>   h <- runCommand "/usr/bin/env runhaskell Database.CouchDB.Tests" 
>   waitForProcess h
>   return ()
