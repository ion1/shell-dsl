# ShellDSL

[![Build Status](https://travis-ci.org/ion1/shell-dsl.svg)](https://travis-ci.org/ion1/shell-dsl) [![Hackage](https://budueba.com/hackage/shell-dsl)](https://hackage.haskell.org/package/shell-dsl)

ShellDSL is a library for the generation of POSIX shell scripts with the
appropriate escaping of unsafe parameters.

Example of usage: (full example in `examples` and in module documentation)

```haskell
main = do
  files <- getDirectoryContents "."
  ...
  Sh.hPut handle (script files)

script paths = Sh.unlines
  [ "#!/bin/sh"
  , "set -eu"
  , Sh.unwords ("ls -dl --" : map Sh.escFilePath paths)
  ]
```
