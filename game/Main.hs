{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.List as List
import           Formatting
import           Formatting.Clock
import           System.Clock
import           System.Environment
import           System.FilePath
import           System.Process.Typed
import qualified Data.List as List
import System.Directory
import qualified Data.ByteString.Lazy as L
main = do
  name:_ <- getArgs
  let sourcefp = dir </> addHs name
  exists <- doesFileExist sourcefp
  if exists
    then do
      putStrLn ("Saving to Git ...")
      runProcess_ (proc "git" ["add",sourcefp])
      runProcess_ (proc "git" ["commit"])
      hash <- fmap L.toStrict (readProcessStdout_ (proc "git" ["rev-parse", "--verify", "HEAD"]))
      putStrLn ("Saved as: " ++ S8.unpack hash ++ "")
      putStrLn ("Compiling with GHC ...")
      runProcess_ (proc "ghc" [sourcefp, "-i", dir, "-o", S8.unpack hash, "-O2", "-fforce-recomp", "-Wall"])
      putStrLn "Running ..."
      start <- getTime Monotonic
      runProcess_ (proc ("./" <> S8.unpack hash) [])
      end <- getTime Monotonic
      fprint ("Successful run complete in: " % timeSpecs) start end
    else error ("Game not found in " ++ dir)
dir = "games/"
dropHs name = maybe name reverse $ List.stripPrefix ".hs" (reverse name)
addHs name =
  if List.isSuffixOf ".hs" name
    then name
    else name ++ ".hs"
