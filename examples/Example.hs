{-# LANGUAGE OverloadedStrings #-}

import qualified System.ShellDSL as Sh
import System.Directory
import System.IO

main = do
  files <- getDirectoryContents "."
  withFile "example.sh" WriteMode $ \handle -> do
    hSetBinaryMode handle True
    hSetBuffering handle (BlockBuffering Nothing)
    Sh.hPut handle (script files)

script paths = Sh.unlines
  [ "#!/bin/sh"
  , "set -eu"
  , Sh.unwords ("ls -dl --" : map Sh.escFilePath paths)
  ]
