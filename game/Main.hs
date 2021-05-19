{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import           Data.Time
import           Formatting
import           Formatting.Clock
import           Formatting.Time
import           System.Clock
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Process.Typed
main = do
  name:_ <- getArgs
  let sourcefp = dir </> addHs name
  exists <- doesFileExist sourcefp
  if exists
    then do
      runProcess_ (proc "git" ["add", sourcefp])
      out <-
        fmap
          (S8.concat . S8.words . L.toStrict)
          (readProcessStdout_ (proc "git" ["diff-index", "--quiet", "HEAD"]))
      let getHash =
            fmap
              (((S8.pack name <> "-") <>) . S.concat . S8.words . L.toStrict)
              (readProcessStdout_ (proc "git" ["rev-parse", "--verify", "HEAD"]))
      hash <-
        if S.null out
          then getHash
          else do
            putStrLn ("Saving to Git ...")
            runProcess_ (proc "git" ["commit", "-m", "Updated: " ++ sourcefp])
            hash <- getHash
            putStrLn ("Saved as: " ++ S8.unpack hash ++ "")
            pure hash
      putStrLn ("Compiling with GHC ...")
      runProcess_
        (proc
           "ghc"
           [ sourcefp
           , "-i" ++ dir
           , "-o"
           , S8.unpack hash
           , "-O2"
           , "-fforce-recomp"
           , "-Wall"
           ])
      let resultDir = "results" </> S8.unpack hash
      createDirectoryIfMissing True resultDir
      putStrLn ("Running in directory " ++ resultDir ++ "...")
      outfile <- openFile (resultDir </> "stdout") WriteMode
      errfile <- openFile (resultDir </> "stderr") WriteMode
      statsfile <- openFile (resultDir </> "stats") WriteMode
      now <- getCurrentTime
      hprint statsfile ("Starting at " % datetime % "\n") now
      start <- getTime Monotonic
      pwd <- getCurrentDirectory
      runProcess_
        (setStderr
           (useHandleClose errfile)
           (setStdout
              (useHandleClose outfile)
              (setWorkingDir resultDir (proc (pwd </> S8.unpack hash) []))))
      end <- getTime Monotonic
      now <- getCurrentTime
      hprint statsfile ("Ending at " % datetime % "\n") now
      hprint
        statsfile
        ("Successful run complete in: " % timeSpecs % "\n")
        start
        end
      hClose statsfile
    else error ("Game not found in " ++ dir)
dir = "games/"
dropHs name = maybe name reverse $ List.stripPrefix ".hs" (reverse name)
addHs name =
  if List.isSuffixOf ".hs" name
    then name
    else name ++ ".hs"
