{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

-- | Fast CSV writing; using less memory.

module FastCsvVerboseOutput where

import qualified Data.ByteString.Builder as SB
import           Data.Proxy
import           Engine.QLearningVerboseOutput (Env)
import qualified Engine.QLearningVerboseOutput as QLearning
import           Engine.TLL
import           System.IO (hSetBinaryMode)
import           UnliftIO

class BuildHeaders r where
  buildHeaders :: Proxy r -> SB.Builder

class BuildHeaders r => BuildCsvRow r where
  buildCsvRow :: r -> SB.Builder

class BuildCsvField f where
  buildCsvField :: f -> SB.Builder

{-# INLINE withCsvFile #-}
withCsvFile ::
     forall r m. (BuildCsvRow r, MonadUnliftIO m)
  => FilePath
  -- ^ Path to write CSV.
  -> ((r -> IO ()) -> m ())
  -- ^ Get a row-writing function.
  -> m ()
withCsvFile fp cont =
  UnliftIO.withFile
    fp
    WriteMode
    (\handle' -> do
       liftIO
         (do hSetBinaryMode handle' True
             hSetBuffering handle' (BlockBuffering Nothing)
             SB.hPutBuilder handle' (buildHeaders (Proxy :: Proxy r) <> "\n"))
       cont (\row -> SB.hPutBuilder handle' (buildCsvRow row <> "\n")))

{-# INLINE withCsvFileQMatrix #-}
withCsvFileQMatrix ::
     forall r m a n o . (BuildCsvRow r, MonadUnliftIO m)
  => FilePath
  -- ^ Path to write CSV.
  -> ((r -> IO ()) -> m (List '[ ( a , Env n o a), ( a , Env n o a)]))
  -- ^ Get a row-writing function.
  -> m (List '[ ( a , Env n o a), ( a , Env n o a)])
withCsvFileQMatrix fp cont =
  UnliftIO.withFile
    fp
    WriteMode
    (\handle' -> do
       liftIO
         (do hSetBinaryMode handle' True
             hSetBuffering handle' (BlockBuffering Nothing)
             SB.hPutBuilder handle' (buildHeaders (Proxy :: Proxy r) <> "\n"))
       cont (\row -> SB.hPutBuilder handle' (buildCsvRow row <> "\n")))
