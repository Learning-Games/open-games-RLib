{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
module Main where
import           Control.Concurrent
import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Data.Foldable
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.String
import           Data.Time
import           Formatting
import qualified Formatting.Clock as F
import qualified Formatting.Time as F
import           Path
import qualified Path.IO as IO
import           Prelude hiding (putStrLn)
import           System.Clock
import           System.Environment
import           System.Exit
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
   \  stack run upload MODULE\n\
   \\n\
   \    Copy the module remotely. Will be committed, compiled & run by the\n\
   \    remote server.\n\
   \\n\
   \  stack run jobs MODULE\n\
   \\n\
   \    Get the set of jobs by the name MODULE.\n\
   \\n\
   \  stack run logs JOB\n\
   \\n\
   \    Get the logs for the job from the remote server.\n\
   \\n\
   \For running on the remote cloud service:\n\
   \\n\
   \  stack run watch\n\
   \\n\
   \    Watches for new Haskell files.\n\
   \\n\
   \    Commits, compiles & run the module."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  root <- IO.getCurrentDir
  gamesDir <- IO.makeAbsolute gamesRelDir
  case args of
    ["local", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runLocal root gamesDir file (normalizeName (dropHs file0))
    ["check", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runCheck file
    ["watch"] -> do
      runWatch
    ["upload", file0] -> do
      file <- IO.resolveFile gamesDir (addHs file0)
      runUpload file
    ["logs", file0] -> do
      runLogs (normalizeName (dropHs file0))
    _ -> do
      error helpString

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
-- Print the logs

runLogs :: Name -> IO ()
runLogs name = do
  runProcess_ (proc "ssh" [serverHost, "echo", "Connection to server OK."])
  jobsString <-
    readProcessStdout_
      (proc
         "ssh"
         [serverHost, "ls", toFilePath (learningWorkDir </> resultsRelDir)])
  let jobsList = map (HashedName . S8.unpack) (S8.lines (L.toStrict jobsString))
  traverse_
    (\job ->
       when
         (getHashedName job == name)
         (do putStrLn ("Logs for job " ++ unHashedName job)
             let dump label = do
                   putStrLn label
                   runProcess_
                     (proc
                        "ssh"
                        [ serverHost
                        , "cat"
                        , toFilePath (learningWorkDir </> resultsRelDir) ++
                          unHashedName job ++ "/" ++ label
                        ])
             dump "stdout"
             dump "stderr"
             dump "stats"
             putStrLn
               (unlines
                  [ "Run the following to download the complete directory of the job: "
                  , ""
                  , "scp -r " ++
                    serverHost ++
                    ":" ++
                    toFilePath (learningWorkDir </> resultsRelDir) ++
                    unHashedName job ++ " ."
                  ])))
    jobsList

--------------------------------------------------------------------------------
-- Run locally

runLocal :: Path Abs Dir -> Path Abs Dir -> Path Abs File -> Name -> IO ()
runLocal rootDir gamesDir sourceFile name = do
  exists <- IO.doesFileExist sourceFile
  if not exists
    then error ("Game not found: " ++ toFilePath sourceFile)
    else do
      runProcess_
        (setWorkingDir
           (toFilePath gamesDir)
           (proc "git" ["add", toFilePath sourceFile]))
      code <-
        runProcess
          (setWorkingDir
             (toFilePath gamesDir)
             (proc "git" ["diff-index", "--quiet", "HEAD"]))
      let getHash =
            fmap
              (addHash name)
              (readProcessStdout_
                 (setWorkingDir
                    (toFilePath gamesDir)
                    (proc "git" ["rev-parse", "--verify", "HEAD"])))
      hashedName <-
        if code == ExitSuccess
          then do
            putStrLn "No change to source, we'll just re-run it."
            getHash
          else do
            putStrLn ("Saving to Git ...")
            runProcess_
              (setWorkingDir
                 (toFilePath gamesDir)
                 (proc "git" ["commit", "-m", "Updated: " ++ unName name]))
            _ <- runProcess (setWorkingDir (toFilePath gamesDir) (proc "git" ["push"]))
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
              hprint statsfile ("Starting at " % F.datetime % "\n") startTime
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
                    ("Successful end at " % F.datetime % "\n")
                    endTime
                  hprint
                    statsfile
                    ("Running time: " % F.timeSpecs % "\n")
                    start
                    end
                else do
                  hprint
                    statsfile
                    ("Exited with failure at " % F.datetime % "\n")
                    endTime
                  hprint
                    statsfile
                    ("Exited code: " % string % "\n")
                    (show status)
                  hprint
                    statsfile
                    ("Running time so far: " % F.timeSpecs % "\n")
                    start
                    end
        putStrLn ("Run complete in directory " ++ toFilePath resultDir)

--------------------------------------------------------------------------------
-- Watcher

runWatch ::  IO ()
runWatch = do
  void (watchDir learningWatchDir action)
  putStrLn ("Content directory: " <> toFilePath learningWorkDir)
  putStrLn
    ("Watching games directory " <> toFilePath learningWatchDir <> " ...")
  forever (threadDelay 1000000)
  where
    action originFile =
      void
        (forkIO
           (do putStrLn
                 ("Incoming file " ++ toFilePath (filename originFile))
               let targetGamesDir = learningWorkDir </> gamesRelDir
                   finalFile = targetGamesDir </> filename originFile
               IO.createDirIfMissing True targetGamesDir
               IO.copyFile originFile finalFile
               IO.withCurrentDir
                 learningWorkDir
                 (runLocal
                    learningWorkDir
                    targetGamesDir
                    finalFile
                    (normalizeName
                       (dropHs (toFilePath (filename finalFile)))))))

watchDir :: Path Abs Dir -> (Path Abs File -> IO ()) -> IO ()
watchDir dir action = do
  putStrLn "Polling for changes ..."
  loop mempty
  where
    loop previous = do
      threadDelay (1000 * 1000 * pollSeconds)
      (_dirs, files) <-
        fmap
          (second (filter (isHaskell . toFilePath . filename)))
          (IO.listDir dir)
      current <-
        fmap
          Set.fromList
          (traverse
             (\file -> do
                time <- IO.getModificationTime file
                pure (file, time))
             files)
      let new = Set.difference current previous
      unless (Set.null previous) (traverse_ action (map fst (Set.toList new)))
      loop current

--------------------------------------------------------------------------------
-- Name munging

isHaskell :: [Char] -> Bool
isHaskell x = dropHs x /= x

dropHs :: [Char] -> [Char]
dropHs name = maybe name reverse $ List.stripPrefix (reverse ".hs") (reverse name)

addHs :: [Char] -> [Char]
addHs name =
  if List.isSuffixOf ".hs" name
    then name
    else name ++ ".hs"

--------------------------------------------------------------------------------
-- Module name

newtype Name = Name {unName :: String} deriving (Show, Eq)

normalizeName :: String -> Name
normalizeName =
  Name .
  map
    (\c ->
       if isAlphaNum c || c == '_' || c == '-'
         then c
         else '_')

newtype HashedName = HashedName {unHashedName :: String} deriving (Show)

addHash :: Name -> L.ByteString -> HashedName
addHash (Name name) =
  HashedName . ((name <> "-") <>) . S8.unpack . S.concat . S8.words . L.toStrict

getHashedName :: HashedName -> Name
getHashedName (HashedName x) = Name (take (length x - 41 {-includes the dash-}) x)

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

learningWorkDir :: Path Abs Dir
learningWorkDir = $(mkAbsDir "/root/learning-work")

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

pollSeconds :: Int
pollSeconds = 5