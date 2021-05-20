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
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process.Typed
main = do
  args <- getArgs
  case args of
    ["local", file] -> runLocal file
    _ ->
      error
        "Arguments expected:\n\
         \\n\
         \stack run local MODULE\n\
         \\n\
         \  Commit, compile & run the module locally.\n\
         \\n\
         \stack run check MODULE\n\
         \\n\
         \  Compile the module locally.\n\
         \\n\
         \stack run remote MODULE\n\
         \\n\
         \  Copy the module remotely, then commit, compile & run it on the\n\
         \  remote server.\n\
         \"

runLocal name = do
  let sourcefp = dir </> addHs name
  exists <- doesFileExist sourcefp
  if exists
    then do
      runProcess_ (proc "git" ["add", sourcefp])
      code <- runProcess (proc "git" ["diff-index", "--quiet", "HEAD"])
      let getHash =
            fmap
              (((S8.pack name <> "-") <>) . S.concat . S8.words . L.toStrict)
              (readProcessStdout_ (proc "git" ["rev-parse", "--verify", "HEAD"]))
      hash <-
        if code == ExitSuccess
          then do putStrLn "No change to source, we'll just re-run it."
                  getHash
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
      withFile (resultDir </> "stdout") WriteMode $ \outfile ->
        withFile (resultDir </> "stderr") WriteMode $ \errfile ->
          withFile (resultDir </> "stats") WriteMode $ \statsfile -> do
            now <- getCurrentTime
            hprint statsfile ("Starting at " % datetime % "\n") now
            start <- getTime Monotonic
            pwd <- getCurrentDirectory
            status <-
              runProcess
                (setStderr
                   (useHandleClose errfile)
                   (setStdout
                      (useHandleClose outfile)
                      (setWorkingDir
                         resultDir
                         (proc (pwd </> S8.unpack hash) []))))
            end <- getTime Monotonic
            now <- getCurrentTime
            if status == ExitSuccess
              then do
                hprint statsfile ("Successful end at " % datetime % "\n") now
                hprint statsfile ("Running time: " % timeSpecs % "\n") start end
              else do
                hprint
                  statsfile
                  ("Exited with failure at " % datetime % "\n")
                  now
                hprint statsfile ("Exited code: " % string % "\n") (show status)
                hprint
                  statsfile
                  ("Running time so far: " % timeSpecs % "\n")
                  start
                  end
    else error ("Game not found in " ++ dir)


dir = "games/"
dropHs name = maybe name reverse $ List.stripPrefix ".hs" (reverse name)
addHs name =
  if List.isSuffixOf ".hs" name
    then name
    else name ++ ".hs"
