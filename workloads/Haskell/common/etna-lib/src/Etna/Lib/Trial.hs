{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Etna.Lib.Trial (run, sample) where

import Etna.Lib.Types (Result (Result))
import qualified Etna.Lib.Types as B
import Control.Monad (forM)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 as B8 (appendFile)
import Data.Char (toLower)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Clock (Clock (..), getTime, toNanoSecs)
import System.IO.Silently (silence)
import System.TimeIt (timeItT)
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BL

import Test.QuickCheck hiding (Result, sample)
import Test.QuickCheck.Property hiding (Result)

import Data.IORef
import Data.Time.Clock
import Text.Printf

import Control.Monad
import Data.List



data FullResult = FullResult
  { workload :: String,
    strategy :: String,
    property :: String,
    status :: String,
    tests :: Maybe Int,
    discards :: Maybe Int,
    time :: String,
    counterexample :: String
  }
  deriving (Generic)

instance ToJSON FullResult

type Timeout = Maybe Double

type Info = (String, String, String)

runOne :: Info -> Timeout -> IO Result -> IO FullResult
runOne (workload, strategy, property) mtimeout test = do
  case mtimeout of
    Nothing -> run
    Just t -> fromMaybe (defaultResult (printf "%.6fs" t)) <$> timeout (fromSec t) run
  where
    run = do
      (time, Result {..}) <- myTimeIt $ eval $ silence test
      return FullResult {tests = Just tests, time = printf "%.6fs" time, ..}

    fromSec :: Double -> Int
    fromSec = round . (1000000 *)

    -- Returned if the trial timed out
    defaultResult time =
      FullResult
        { status = "Timed Out",
          tests = Nothing,
          discards = Nothing,
          counterexample = "",
          ..
        }

-- Based on `System.TimeIt`
myTimeIt :: IO a -> IO (Double, a)
myTimeIt ioa = do
  mt1 <- getTime Monotonic
  a <- ioa
  mt2 <- getTime Monotonic
  let t t2 t1 = fromIntegral (toNanoSecs t2 - toNanoSecs t1) * 1e-9
  return (t mt2 mt1, a)

-- Force evaluation (avoid laziness problems).
eval :: IO Result -> IO Result
eval ia = do
  Result {..} <- ia
  return Result {..}
{-# NOINLINE eval #-}

run :: Info -> Timeout -> IO Result -> IO ()
run info timeout test = do
  result <- runOne info timeout test
  putStrLn ("[|" ++ BL.unpack (encode result) ++ "|]")
  -- B8.appendFile file (encode result)
  -- Prelude.appendFile file "\n"



sample :: Int -> Property -> IO ()
sample tests property = quickSample tests property
  -- B8.appendFile file (encode result)
  -- Prelude.appendFile file "\n"



quickSample :: Int -> Property -> IO ()
quickSample n p = do
  let args = stdArgs{maxSuccess = n, chatty = False}
  ins <- newIORef []
  t <- getCurrentTime
  quickCheckWith args (callback (PostTest NotCounterexample (\_ res -> do
                                                                   t' <- getCurrentTime
                                                                   modifyIORef ins ((t', testCase res):))) p)
  cs <- readIORef ins
  tr <- newIORef t
  opts <- forM (reverse cs) $ \(t', c) -> do
    t0 <- readIORef tr
    writeIORef tr t'
    return ("{ \"time\" : \"" ++ show (diffUTCTime t' t0) ++ "\"," ++
            "  \"value\": \"" ++ unwords c ++ "\"}")
  putStrLn ("[" ++ intercalate ", " opts ++ "]")