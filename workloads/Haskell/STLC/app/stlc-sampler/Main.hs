{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Etna.Lib
import Data.List (lookup)
import Data.Maybe
import Strategy.Correct as Correct
-- import Strategy.Lean as Lean
-- import Strategy.LeanRev as LeanRev
-- import Strategy.Quick as Quick
-- import Strategy.Size as Size
-- import Strategy.Small as Small
-- import Strategy.SmallRev as SmallRev
import Impl (Expr (..), Typ(..), Ctx)

import System.Environment (getArgs)
import Control.DeepSeq
import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Test.QuickCheck hiding (sample)

type Sample i = i -> Bool 


instance NFData Typ where
    rnf TBool = ()
    rnf (TFun t1 t2) = rnf t1 `seq` rnf t2

instance NFData Expr where
    rnf (Var n) = rnf n
    rnf (Bool b) = rnf b
    rnf (Abs t e) = rnf t `seq` rnf e
    rnf (App e1 e2) = rnf e1 `seq` rnf e2


prop_SinglePreserve :: Sample Expr
prop_SinglePreserve e =
  deepseq e True

prop_MultiPreserve :: Sample Expr
prop_MultiPreserve e =
  deepseq e True


sample_SinglePreserve = forAll arbitrary prop_SinglePreserve
sample_MultiPreserve = forAll arbitrary prop_MultiPreserve


main :: IO ()
main
  = do args <- getArgs
       let SampleArgs strategy prop tests
             = parseSampleArgs (head args)
           test
             = fromJust $ lookup (strategy, prop) mmap
       sample tests test
mmap :: [((String, String), Property)]
mmap
  = [(("Correct", "prop_SinglePreserve"), sample_SinglePreserve),
     (("Correct", "prop_MultiPreserve"), sample_MultiPreserve)]