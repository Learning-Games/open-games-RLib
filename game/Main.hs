{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
module Main where
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import qualified Data.List as List
import           Data.String
import           Data.Time
import           Formatting
import           Formatting.Clock
import           Formatting.Time
import           Path
import qualified Path.IO as IO
import           Prelude hiding (putStrLn)
import           System.Clock
import           System.Environment
import           System.Exit
import           System.FSNotify
import           System.IO hiding (putStrLn)
import           System.Process.Typed

--------------------------------------------------------------------------------
-- Main entry point

helpString :: [Char]
helpString =
  "Invalid arguments given.\n\
   \\n\
   \Workflow: \n\
   \\n\
   \  1. First check your module, which should be in the games/ directory.\n\
   \  2. Do a quick sanity check run locally with a small number of\n\
   \     iterations.\n\
   \  3. When ready, run remotely with desired number of iterations.\n\
   \\n\
   \See commands list below:\n\
   \\n\
   \  stack run local MODULE\n\
   \\n\
   \    Commit, compile & run the module locally.\n\
   \\n\
   \  stack run check MODULE\n\
   \\n\
   \    Compile the module locally.\n\
   \\n\
   \  stack run remote MODULE\n\
   \\n\
   \    Copy the module remotely, then commit, compile & run it on the\n\
   \    remote server.\n\
   \\n\
   \For running on the remote cloud service:\n\
   \\n\
   \  stack run watch WORKING_DIR GAMES_DIR\n\
   \\n\
   \    Watches for new Haskell files in the directory named.\n\
   \\n\
   \    Commits, compiles & run the module."

main :: IO ()
main = do
  args <- getArgs
  root <- IO.getCurrentDir
  gamesDir <- IO.makeAbsolute gamesRelDir
  case args of
    ["local", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runLocal root file (normalizeName (dropHs file0))
    ["check", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runCheck file
    ["watch", root0] -> do
      root' <- parseAbsDir root0
      runWatch root'
    ["upload", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runUpload file
    _ -> error helpString

--------------------------------------------------------------------------------
-- Quick compilation check

runCheck :: Path Abs File -> IO ()
runCheck sourceFile = do
  exists <- IO.doesFileExist sourceFile
  if not exists
    then error ("Game not found: " ++ toFilePath sourceFile)
    else do
      putStrLn ("Checking with GHC ...")
      IO.withSystemTempDir "game" $ \tmpDir -> do
        runProcess_
          (setWorkingDir
             (toFilePath tmpDir)
             (proc
                "ghc"
                [ toFilePath sourceFile
                , "-no-link"
                , "-O0"
                , "-Wall"
                , "-fforce-recomp"
                ]))

--------------------------------------------------------------------------------
-- Upload remotely

runUpload :: Path Abs File -> IO ()
runUpload sourceFile = do
  exists <- IO.doesFileExist sourceFile
  if not exists
    then error ("Game not found: " ++ toFilePath sourceFile)
    else do
      runProcess_
        (proc "ssh" [serverHost, "echo", "Connection to server OK."])
      runProcess_
        (proc
           "ssh"
           [serverHost, "mkdir", "-p", toFilePath learningWatchDir])
      let path = learningWatchDir </> filename sourceFile
      putStrLn "Uploading to service ..."
      -- Copy
      runProcess_
        (proc
           "scp"
           [ toFilePath sourceFile
           , serverHost <> ":" <> toFilePath path <> ".tmp"
           ])
      -- Atomic rename
      runProcess_
        (proc
           "ssh"
           [serverHost, "mv", toFilePath path <> ".tmp", toFilePath path])
      putStrLn "Uploaded."

--------------------------------------------------------------------------------
-- Run locally

runLocal :: Path Abs Dir -> Path Abs File -> Name -> IO ()
runLocal rootDir sourceFile name = do
  exists <- IO.doesFileExist sourceFile
  if not exists
    then error ("Game not found: " ++ toFilePath sourceFile)
    else do
      runProcess_ (proc "git" ["add", toFilePath sourceFile])
      code <- runProcess (proc "git" ["diff-index", "--quiet", "HEAD"])
      let getHash =
            fmap
              (addHash name)
              (readProcessStdout_ (proc "git" ["rev-parse", "--verify", "HEAD"]))
      hashedName <-
        if code == ExitSuccess
          then do
            putStrLn "No change to source, we'll just re-run it."
            getHash
          else do
            putStrLn ("Saving to Git ...")
            runProcess_
              (proc "git" ["commit", "-m", "Updated: " ++ unName name])
            hash <- getHash
            putStrLn ("Saved as: " ++ unHashedName hash)
            pure hash
      putStrLn ("Compiling with GHC ...")
      binaryRelFile <- parseRelFile (unHashedName hashedName)
      let binDir = rootDir </> binRelDir
      IO.createDirIfMissing True binDir
      let binaryAbsFile = binDir </> binaryRelFile
      IO.withSystemTempDir "game" $ \tmpDir -> do
        runProcess_
          (setWorkingDir
             (toFilePath tmpDir)
             (proc
                "ghc"
                [ toFilePath sourceFile
                , "-o"
                , toFilePath binaryAbsFile
                , "-O2"
                , "-fforce-recomp"
                , "-Wall"
                ]))
        hashedNameDir <- parseRelDir (unHashedName hashedName)
        let resultDir = rootDir </> resultsRelDir </> hashedNameDir
        IO.createDirIfMissing True resultDir
        putStrLn ("Running in directory " ++ toFilePath resultDir ++ "...")
        withFile (toFilePath (resultDir </> stdoutFile)) WriteMode $ \outfile ->
          withFile (toFilePath (resultDir </> stderrFile)) WriteMode $ \errfile ->
            withFile (toFilePath (resultDir </> statsFile)) WriteMode $ \statsfile -> do
              startTime <- getCurrentTime
              hprint statsfile ("Starting at " % datetime % "\n") startTime
              start <- getTime Monotonic
              status <-
                runProcess
                  (setStderr
                     (useHandleClose errfile)
                     (setStdout
                        (useHandleClose outfile)
                        (setWorkingDir
                           (toFilePath resultDir)
                           (proc (toFilePath binaryAbsFile) []))))
              end <- getTime Monotonic
              endTime <- getCurrentTime
              if status == ExitSuccess
                then do
                  hprint
                    statsfile
                    ("Successful end at " % datetime % "\n")
                    endTime
                  hprint
                    statsfile
                    ("Running time: " % timeSpecs % "\n")
                    start
                    end
                else do
                  hprint
                    statsfile
                    ("Exited with failure at " % datetime % "\n")
                    endTime
                  hprint
                    statsfile
                    ("Exited code: " % string % "\n")
                    (show status)
                  hprint
                    statsfile
                    ("Running time so far: " % timeSpecs % "\n")
                    start
                    end

--------------------------------------------------------------------------------
-- Watcher

runWatch :: Path Abs Dir -> IO ()
runWatch rootDir =
  withManager
    (\mgr -> do
       void (watchDir mgr (toFilePath learningWatchDir) (isHaskell . eventPath) action)
       putStrLn ("Content directory: " <> toFilePath rootDir)
       putStrLn ("Watching games directory " <> toFilePath learningWatchDir <> " ...")
       forever (threadDelay 1000000))
  where
    isHaskell = List.isPrefixOf (reverse ".hs") . reverse
    action event =
      if eventIsDirectory event
        then pure ()
        else if actionable event
               then void
                      (forkIO
                         (do originFile <- parseAbsFile (eventPath event)
                             putStrLn
                               ("Incoming file " ++
                                toFilePath (filename originFile))
                             let targetGamesDir = rootDir </> gamesRelDir
                                 finalFile =
                                   targetGamesDir </> filename originFile
                             IO.createDirIfMissing True targetGamesDir
                             IO.renameFile originFile finalFile
                             IO.withCurrentDir
                               rootDir
                               (runLocal
                                  rootDir
                                  finalFile
                                  (normalizeName
                                     (dropHs (toFilePath (filename finalFile)))))))
               else pure ()
      where
        actionable =
          \case
            Added {} -> True
            Modified {} -> True
            _ -> False

--------------------------------------------------------------------------------
-- Name munging

dropHs :: [Char] -> [Char]
dropHs name = maybe name reverse $ List.stripPrefix (reverse ".hs") (reverse name)

addHs :: [Char] -> [Char]
addHs name =
  if List.isSuffixOf ".hs" name
    then name
    else name ++ ".hs"

--------------------------------------------------------------------------------
-- Module name

newtype Name = Name {unName :: String}

normalizeName :: String -> Name
normalizeName =
  Name .
  map
    (\c ->
       if isAlphaNum c || c == '_' || c == '-'
         then c
         else '_')

newtype HashedName = HashedName {unHashedName :: String}

addHash :: Name -> L.ByteString -> HashedName
addHash (Name name) =
  HashedName . ((name <> "-") <>) . S8.unpack . S.concat . S8.words . L.toStrict

--------------------------------------------------------------------------------
-- Threaded IO

-- | This is a thread-safe stdout printer, with timestamp.
putStrLn :: String -> IO ()
putStrLn s = do
  threadid <- myThreadId
  now' <- getCurrentTime
  S8.putStrLn
    (S8.pack ("[" ++ filter isDigit (show threadid) <> "] " <> show now' <> ": " <> s))

--------------------------------------------------------------------------------
-- Paths

learningWatchDir :: Path Abs Dir
learningWatchDir = $(mkAbsDir "/tmp/learning-watch")

binRelDir :: Path Rel Dir
binRelDir = $(mkRelDir "bin")

stdoutFile :: Path Rel File
stdoutFile = $(mkRelFile "stdout")

stderrFile :: Path Rel File
stderrFile = $(mkRelFile "stderr")

statsFile :: Path Rel File
statsFile =  $(mkRelFile "stats")

gamesRelDir :: Path Rel Dir
gamesRelDir = $(mkRelDir "games")

resultsRelDir :: Path Rel Dir
resultsRelDir = $(mkRelDir "results")

--------------------------------------------------------------------------------
-- Other constants

-- Make an entry in your ~/.ssh/config like this:
--
--   Host learning-service
--     IdentityFile /home/user/.ssh/id_rsa
--     HostName 1.1.1.1
--     User root
--
-- Replacing the IP address and identity file path appropriately.
serverHost :: IsString s => s
serverHost = "learning-service"
