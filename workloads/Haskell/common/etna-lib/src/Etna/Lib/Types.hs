{-# LANGUAGE DeriveGeneric #-}

module Etna.Lib.Types where

import Data.Aeson (FromJSON)
import Data.Functor
import GHC.Generics

data Result = Result
  { status :: String,
    tests :: Int,
    discards :: Maybe Int,
    counterexample :: String
  }
  deriving (Show)

type Cap = Int

type PropPair = (Bool, Bool) -- (precondition, postcondition)

(-->) :: Bool -> Bool -> PropPair
(-->) = (,)

infixr 0 -->

type Task a = a -> PropPair

type Strategy a = Task a -> IO Result

data Approach = Correct | Naive

data ExpArgs = ExpArgs
  { workload :: String,
    strategy :: String,
    property :: String,
    timeout :: Maybe Double
  }
  deriving (Generic, Show)

instance FromJSON ExpArgs

data SampleArgs = SampleArgs
  { sstrategy :: String,
    sproperty :: String,
    stests    :: Int
  }
  deriving (Generic, Show)

instance FromJSON SampleArgs