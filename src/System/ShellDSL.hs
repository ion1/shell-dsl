{-# LANGUAGE CPP #-}

-- |
-- Module      : System.ShellDSL
-- Description : A library for the generation of shell scripts
-- Copyright   : © 2014 Johan Kiviniemi
-- License     : MIT
-- Maintainer  : Johan Kiviniemi <devel@johan.kiviniemi.name>
-- Stability   : provisional
-- Portability : CPP
--
-- ShellDSL is a library for the generation of POSIX shell scripts with the
-- appropriate escaping of unsafe parameters.
--
-- Example of usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified System.ShellDSL as Sh
-- > import System.Directory
-- > import System.IO
-- >
-- > main = do
-- >   files <- getDirectoryContents "."
-- >   withFile "example.sh" WriteMode $ \handle -> do
-- >     hSetBinaryMode handle True
-- >     hSetBuffering handle (BlockBuffering Nothing)
-- >     Sh.hPut handle (script files)
-- >
-- > script paths = Sh.unlines
-- >   [ "#!/bin/sh"
-- >   , "set -eu"
-- >   , Sh.unwords ("ls -dl --" : map Sh.escFilePath paths)
-- >   ]

module System.ShellDSL
  ( Shell
    -- * Verbatim insertion
  , verbatim, verbatimText, verbatimString
#ifdef __GLASGOW_HASKELL__
  , verbatimFilePath
#endif
    -- * Escaped insertion
  , esc, escText, escString
#ifdef __GLASGOW_HASKELL__
  , escFilePath
#endif
  , -- * Getting the script out
    toBS, hPut
#ifdef __GLASGOW_HASKELL__
  , proc
#endif
  , -- * Other operations
    unlines, unwords, intercalate
  ) where

import Prelude hiding (unlines, unwords)

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Bui
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Foldable (Foldable, foldMap, toList)
import Data.Function
import qualified Data.List as List
import Data.Monoid
import Data.String (IsString (..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as Enc
import Data.Word
import System.IO

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Foreign as GHC
#ifndef mingw32_HOST_OS
import qualified GHC.IO.Encoding as GHC
#else
import qualified GHC.IO.Encoding.Failure as GHC
import qualified GHC.IO.Encoding.UTF8 as GHC
#endif
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Proc
#endif

-- $setup
-- >>> import System.Exit
-- >>> :set -XOverloadedStrings
-- >>> hSetBinaryMode stdout True

-- | A representation of a POSIX shell script.
newtype Shell = Shell Builder

instance IsString Shell where
  fromString = verbatimString
  {-# INLINE fromString #-}

instance Eq Shell where
  (==) = (==) `on` toBS

instance Ord Shell where
  compare = compare `on` toBS

instance Monoid Shell where
  mempty = Shell mempty
  {-# INLINE mempty #-}
  mappend (Shell a) (Shell b) = Shell (mappend a b)
  {-# INLINE mappend #-}

instance Show Shell where
  showsPrec p sh = showParen (p > 10)
                 $ showString "System.ShellDSL.verbatim "
                 . showsPrec 11 (toBS sh)

-- | Insert a known to be safe 'ByteString' verbatim.
--
-- >>> hPut stdout (verbatim "set -eu")
-- set -eu
verbatim :: ByteString -> Shell
verbatim = Shell . Bui.lazyByteString
{-# INLINE verbatim #-}

-- | Insert known to be safe 'Text' verbatim (encoded in UTF-8).
--
-- >>> hPut stdout (verbatimText "printf '%s\\n' \x263a")
-- printf '%s\n' ☺
verbatimText :: Text -> Shell
verbatimText = verbatim . Enc.encodeUtf8
{-# INLINE verbatimText #-}

-- | Insert a known to be safe 'String' verbatim (encoded in UTF-8). Please use
-- 'verbatimFilePath' instead of this for 'FilePath's.
--
-- >>> hPut stdout (verbatimString "printf '%s\\n' \x263a")
-- printf '%s\n' ☺
--
-- >>> hPut stdout "printf '%s\\n' \x263a"  -- OverloadedStrings
-- printf '%s\n' ☺
verbatimString :: String -> Shell
verbatimString = verbatimText . TL.pack
{-# INLINE verbatimString #-}

-- | Escape and insert a 'ByteString'.
--
-- Any 0-bytes will be skipped.
--
-- Also remember to protect against parameters beginning with a dash by using
-- @--@ where appropriate.
--
-- >>> hPut stdout ("ls -l -- " <> esc "file; name")
-- ls -l -- 'file; name'
--
-- >>> hPut stdout ("ls -l -- " <> esc "")
-- ls -l -- ''
--
-- >>> hPut stdout ("ls -l -- " <> esc "apostrophe '")
-- ls -l -- 'apostrophe '\'
esc :: ByteString -> Shell
esc = Shell . escape
{-# INLINE esc #-}

-- | Escape and insert 'Text' (encoded in UTF-8).
--
-- Any 0-codepoints will be skipped.
--
-- Also remember to protect against parameters beginning with a dash by using
-- @--@ where appropriate.
--
-- >>> hPut stdout ("ls -l -- " <> escText "file; name \x263a")
-- ls -l -- 'file; name ☺'
escText :: Text -> Shell
escText = esc . Enc.encodeUtf8
{-# INLINE escText #-}

-- | Escape and insert a 'String' (encoded in UTF-8). Please use
-- 'escFilePath' instead of this for 'FilePath's.
--
-- Any 0-codepoints will be skipped.
--
-- Also remember to protect against parameters beginning with a dash by using
-- @--@ where appropriate.
--
-- >>> hPut stdout ("ls -l -- " <> escString "file; name \x263a")
-- ls -l -- 'file; name ☺'
escString :: String -> Shell
escString = escText . TL.pack
{-# INLINE escString #-}

-- | Output the generated script as a 'ByteString'.
--
-- For writing into a file handle, use 'hPut' instead.
--
-- >>> toBS "printf '%s\\n' \x263a"
-- "printf '%s\\n' \226\152\186"
toBS :: Shell -> ByteString
toBS (Shell b) = Bui.toLazyByteString b
{-# INLINE toBS #-}

-- | Write the generated script into a 'Handle'.
--
-- The Handle should be set to binary and 'BlockBuffering' mode. Please see
-- 'hSetBinaryMode' and 'hSetBuffering'.
--
-- >>> hPut stdout "printf '%s\\n' \x263a"
-- printf '%s\n' ☺
hPut :: Handle -> Shell -> IO ()
hPut h (Shell b) = Bui.hPutBuilder h b
{-# INLINE hPut #-}

-- | Append a newline to each element and join the lines.
--
-- >>> hPut stdout (unlines [ "set -eu", "printf '%s\\n' \x263a" ])
-- set -eu
-- printf '%s\n' ☺
unlines :: Foldable t => t Shell -> Shell
unlines = foldMap (\line -> line <> Shell (Bui.word8 newline))
{-# INLINE unlines #-}

-- | Join the elements with a space.
--
-- >>> hPut stdout (unwords ("ls -l --" : map esc ["foo", "bar", "baz"]))
-- ls -l -- 'foo' 'bar' 'baz'
unwords :: Foldable t => t Shell -> Shell
unwords = intercalate (Shell (Bui.word8 space))
{-# INLINE unwords #-}

-- | Join the elements with the given separator.
--
-- >>> hPut stdout (intercalate "; " [ "set -eu", "printf '%s\\n' \x263a" ])
-- set -eu; printf '%s\n' ☺
intercalate :: Foldable t => Shell -> t Shell -> Shell
intercalate sep = mconcat . List.intersperse sep . toList
{-# INLINE intercalate #-}

escape :: ByteString -> Builder
-- Start with an apostrophe and the inside state so the empty string will be
-- escaped as ''.
escape bsOrig = Bui.word8 apos <> inside bsOrig
  where
    processBS end zero apostrophe other bs =
      case BSL.uncons bs of
        Nothing -> end
        Just (w, bsTail) | w == 0    -> zero bsTail
                         | w == apos -> apostrophe bsTail
        _ -> other

    outside bsFull = processBS end zero apostrophe other bsFull
      where
        end           = mempty
        zero bs       = outside bs  -- Skip.
        apostrophe bs = Bui.word8 bslash <> Bui.word8 apos <> outside bs
        other         = Bui.word8 apos <> inside bsFull

    inside bsFull = processBS end zero apostrophe other bsFull
      where
        end          = Bui.word8 apos
        zero bs      = inside bs  -- Skip.
        apostrophe _ = Bui.word8 apos <> outside bsFull
        other        = Bui.lazyByteString bsSafe <> inside bsUnsafe
          where
            (bsSafe, bsUnsafe) = BSL.break (\w -> w == 0 || w == apos) bsFull

apos, bslash, newline, space :: Word8
apos    = fromIntegral (ord '\'')
bslash  = fromIntegral (ord '\\')
newline = fromIntegral (ord '\n')
space   = fromIntegral (ord ' ')
{-# INLINE apos #-}
{-# INLINE bslash #-}
{-# INLINE newline #-}
{-# INLINE space #-}

#ifdef __GLASGOW_HASKELL__

-- | Insert a 'FilePath' (a value received from certain System.IO operations)
-- verbatim.
--
-- __Are you sure you do not want to escape it?__ Please see 'escFilePath'.
--
-- __Warning:__ the mapping between system file paths and 'FilePath' values is
-- dependent on the runtime locale settings. This outputs the correct 'Shell'
-- path for 'FilePath' values generated by the same process.
--
-- On Darwin and Windows, system file paths use a representation of Unicode.
-- They will be encoded in UTF-8. This is what is expected of shell scripts on
-- Darwin, msys and cygwin.
--
-- On other UNIX-like systems, system file paths are arbitrary byte arrays.
-- They will be used without any encoding. It is up to the user to do the sane
-- thing and use UTF-8.
--
-- >>> hPut stdout ("ls -l -- " <> verbatimFilePath "/tmp/file; name")
-- ls -l -- /tmp/file; name
verbatimFilePath :: FilePath -> Shell
verbatimFilePath = verbatim . filePathToByteString
{-# INLINE verbatimFilePath #-}

-- | Escape and insert a 'FilePath' (a value received from certain System.IO
-- operations).
--
-- __Warning:__ the mapping between system file paths and 'FilePath' values is
-- dependent on the runtime locale settings. This outputs the correct 'Shell'
-- path for 'FilePath' values generated by the same process.
--
-- On Darwin and Windows, system file paths use a representation of Unicode.
-- They will be encoded in UTF-8. This is what is expected of shell scripts on
-- Darwin, msys and cygwin. Any 0-codepoints will be skipped.
--
-- On other UNIX-like systems, system file paths are arbitrary byte arrays.
-- They will be used without any encoding. It is up to the user to do the sane
-- thing and use UTF-8. Any 0-bytes will be skipped.
--
-- Also remember to protect against parameters beginning with a dash by using
-- @--@ where appropriate.
--
-- >>> hPut stdout ("ls -l -- " <> escFilePath "/tmp/file; name")
-- ls -l -- '/tmp/file; name'
escFilePath :: FilePath -> Shell
escFilePath = esc . filePathToByteString
{-# INLINE escFilePath #-}

-- | Construct a 'Proc.CreateProcess' record for passing to
-- 'Proc.createProcess', representing the command @sh -c <cmd>@ where @<cmd>@
-- is the generated shell script.
--
-- >>> :{
--   do (_,_,_,ph)  <- Proc.createProcess (proc ("printf '%s\\n' \x263a"))
--      ExitSuccess <- Proc.waitForProcess ph
--      return ()
-- :}
-- ☺
proc :: Shell -> Proc.CreateProcess
-- System.Process uses the filesystem encoding for the parameters as well.
proc sh = Proc.proc "sh" [ "-c", byteStringToFilePath (toBS sh) ]

filePathToByteString :: FilePath -> ByteString
filePathToByteString = unsafePerformIO . filePathToByteString'
{-# NOINLINE filePathToByteString #-}

byteStringToFilePath :: ByteString -> FilePath
byteStringToFilePath = unsafePerformIO . byteStringToFilePath'
{-# NOINLINE byteStringToFilePath #-}

filePathToByteString' :: FilePath -> IO ByteString
filePathToByteString' fp = do
  enc <- fileSystemEncoding
  BSL.fromStrict <$> GHC.withCStringLen enc fp (evaluate <=< BS.packCStringLen)

byteStringToFilePath' :: ByteString -> IO FilePath
byteStringToFilePath' bs = do
  enc <- fileSystemEncoding
  BS.unsafeUseAsCStringLen (BSL.toStrict bs)
                           (evaluate <=< GHC.peekCStringLen enc)

fileSystemEncoding :: IO GHC.TextEncoding
#ifndef mingw32_HOST_OS
fileSystemEncoding = GHC.getFileSystemEncoding
#else
fileSystemEncoding = GHC.mkUTF8 GHC.RoundtripFailure
#endif

#endif
