{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ShellDSLTests (tests) where

import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Monoid
import Data.String (IsString (..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as Enc
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.QuickCheck.Instances ()  -- instance Arbitrary ByteString
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty
import Test.Tasty.QuickCheck

#if defined(__GLASGOW_HASKELL__) && !defined(mingw32_HOST_OS) && !defined(darwin_HOST_OS)
import qualified System.Posix.ByteString as Posix
#endif

import System.ShellDSL (Shell)
import qualified System.ShellDSL as Sh

tests :: TestTree
tests =
  testGroup "System.ShellDSL"
    [ testProperty "verbatim/ByteString" $ \bs ->
        Sh.toBS (Sh.verbatim bs) == bs
    , testProperty "verbatim/Text" $ \text ->
        Sh.toBS (Sh.verbatimText text) == encodeText text
    , testProperty "verbatim/String" $ \str ->
        Sh.toBS (Sh.verbatimString str) == encodeString str
    , testProperty "verbatim/IsString" $ \str ->
        Sh.toBS (fromString str) == encodeString str

    , testProperty "append" $ \bsA bsB ->
        Sh.toBS (Sh.verbatim bsA <> Sh.verbatim bsB) == (bsA <> bsB)

    , testProperty "esc/ByteString" $ \bs ->
        printOutputMatches (Sh.esc bs) bs
    , testProperty "esc/Text" $ \text ->
        printOutputMatches (Sh.escText text) (encodeText text)
    , testProperty "esc/String" $ \str ->
        printOutputMatches (Sh.escString str) (encodeString str)

    , testProperty "hPut" $ \bs -> QC.monadicIO $ do
        out <- QC.run . withSystemTempFile "shell-dsl-tests.output" $ \path handle -> do
          hBinaryBlockMode handle
          Sh.hPut handle (Sh.verbatim bs)
          hClose handle

          evaluate . force =<< BSL.readFile path

        QC.assert (out == bs)

#if defined(__GLASGOW_HASKELL__) && !defined(mingw32_HOST_OS) && !defined(darwin_HOST_OS)
    , -- Generate filenames with both valid and invalid UTF-8. Use a
      -- System.IO-style action to read the name into a FilePath and make sure
      -- ShellDSL encodes that correctly into a script.
      testProperty "escFilePath" $ \nameText nameBS -> QC.monadicIO $ do
        -- nameText will encode into valid UTF-8, nameBS may contain bytes that
        -- make it invalid.
        let name = BSL.filter (\w -> w /= 0 && w /= fromIntegral (ord '/'))
                 $ "test text=" <> encodeText nameText <> " bs=" <> nameBS

        result <- QC.run . withSystemTempDirectory "shell-dsl-tests" $ \tempdir -> do
          -- Assuming tempdir is ASCII-only. This may break if it is not.
          let path = encodeString tempdir <> "/" <> name
          bracket (Posix.createFile (BSL.toStrict path) 0o644) Posix.closeFd $ \_ ->
            return ()

          seenNames <- getDirectoryContents tempdir
          let seenPaths = [ tempdir <> "/" <> seenName
                          | seenName@('t':'e':'s':'t':_) <- seenNames
                          ]
              sh = Sh.unlines [ "set -eu"
                              , Sh.unwords ("stat -c '%n' --" : map Sh.escFilePath seenPaths)
                              ]
          out <- runScriptDirectly sh
          return (out == (path <> "\n"))

        QC.assert result
#endif
    ]

encodeText :: Text -> ByteString
encodeText = Enc.encodeUtf8

encodeString :: String -> ByteString
encodeString = encodeText . TL.pack

printOutputMatches :: Shell -> ByteString -> Property
printOutputMatches sh bs = outputMatches shScript bsExpect
  where
    shScript = Sh.unlines [ "set -eu"
                          , Sh.unwords [ "printf 'BEGIN %s END\\n'", sh ]
                          ]
    bsExpect = "BEGIN " <> BSL.filter (/= 0) bs <> " END\n"

outputMatches :: Shell -> ByteString -> Property
outputMatches sh bs = counterexample msg . QC.monadicIO $ do
  func <- QC.pick arbitrary >>= \b -> return $ case b of
            False -> runScriptDirectly
            True  -> runScriptFromFile
  out <- QC.run (func sh)
  QC.assert (out == bs)
  where
    msg = "script: " ++ show sh ++ ", expected output: " ++ show bs

runScriptDirectly :: Shell -> IO ByteString
runScriptDirectly sh =
   readProcessBS ((Sh.proc sh){ std_out = CreatePipe })

runScriptFromFile :: Shell -> IO ByteString
runScriptFromFile sh =
  withSystemTempFile "shell-dsl-tests.sh" $ \path handle -> do
    hBinaryBlockMode handle
    Sh.hPut handle sh
    hClose handle

    readProcessBS (proc "sh" [path])

readProcessBS :: CreateProcess -> IO ByteString
readProcessBS pr = do
  (Nothing, Just outh, Nothing, ph) <-
    createProcess (pr{ std_out = CreatePipe })

  hBinaryBlockMode outh
  out <- evaluate . force =<< BSL.hGetContents outh

  ExitSuccess <- waitForProcess ph

  return out

hBinaryBlockMode :: Handle -> IO ()
hBinaryBlockMode handle = do
  hSetBinaryMode handle True
  hSetBuffering handle (BlockBuffering Nothing)
