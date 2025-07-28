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
import Impl (Tree (..), Color (..))
import Spec (RBT, Key, Val)

import System.Environment (getArgs)
import Control.DeepSeq
import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Test.QuickCheck hiding (sample)

type Sample i = i -> Bool 

instance NFData Key where
  rnf k = k `seq` ()

instance NFData Val where
  rnf v = v `seq` ()

instance NFData Color where
  rnf R = ()
  rnf B = ()

instance (NFData k, NFData v) => NFData (Tree k v) where
  rnf E = ()
  rnf (T c l k v r) = rnf c `seq` rnf k `seq` rnf v `seq` rnf l `seq` rnf r


data Arg2 a b = Arg2 a b
instance (Show a, Show b) => Show (Arg2 a b) where
  show (Arg2 a b) = "(" ++ show a ++ " " ++ show b ++ ")"
instance (Arbitrary a, Arbitrary b) => Arbitrary (Arg2 a b) where
  arbitrary = liftM2 Arg2 arbitrary arbitrary


data Arg3 a b c = Arg3 a b c
instance (Show a, Show b, Show c) => Show (Arg3 a b c) where
  show (Arg3 a b c) = "(" ++ show a ++ " "++ show b ++ " " ++ show c ++ ")"
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Arg3 a b c) where
  arbitrary = liftM3 Arg3 arbitrary arbitrary arbitrary


data Arg4 a b c d = Arg4 a b c d
instance (Show a, Show b, Show c, Show d) => Show (Arg4 a b c d) where
  show (Arg4 a b c d) = "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")"
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Arg4 a b c d) where
  arbitrary = liftM4 Arg4 arbitrary arbitrary arbitrary arbitrary

data Arg5 a b c d e = Arg5 a b c d e
instance (Show a, Show b, Show c, Show d, Show e) => Show (Arg5 a b c d e) where
  show (Arg5 a b c d e) = "(" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ ")"
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e) => Arbitrary (Arg5 a b c d e) where
  arbitrary = liftM5 Arg5 arbitrary arbitrary arbitrary arbitrary arbitrary

prop_InsertValid :: Sample (Arg3 RBT Key Val)
prop_InsertValid (Arg3 t k v) =
  deepseq (t,k,v) True

prop_DeleteValid :: Sample (Arg2 RBT Key)
prop_DeleteValid (Arg2 t k) =
  deepseq (t,k) True

----------

-- Postcondition properties.

prop_InsertPost :: Sample (Arg4 RBT Key Key Val)
prop_InsertPost (Arg4 t k k' v) =
  deepseq (t, k, k', v) True

prop_DeletePost :: Sample (Arg3 RBT Key Key)
prop_DeletePost (Arg3 t k k') =
  deepseq (t, k, k') True

----------

-- Model-based properties.

prop_InsertModel :: Sample (Arg3 RBT Key Val)
prop_InsertModel (Arg3 t k v) =
  deepseq (t, k, v) True

prop_DeleteModel :: Sample (Arg2 RBT Key)
prop_DeleteModel (Arg2 t k) =
  deepseq (t, k) True

----------

-- Metamorphic properties.

prop_InsertInsert :: Sample (Arg5 RBT Key Key Val Val)
prop_InsertInsert (Arg5 t k k' v v') =
  deepseq (t, k, k', v, v') True

prop_InsertDelete :: Sample (Arg4 RBT Key Key Val)
prop_InsertDelete (Arg4 t k k' v) =
  deepseq (t, k, k', v) True

prop_DeleteInsert :: Sample (Arg4 RBT Key Key Val)
prop_DeleteInsert (Arg4 t k k' v') =
  deepseq (t, k, k', v') True

prop_DeleteDelete :: Sample (Arg3 RBT Key Key)
prop_DeleteDelete (Arg3 t k k') =
  deepseq (t, k, k') True


sample_InsertValid = forAll arbitrary prop_InsertValid
sample_DeleteValid = forAll arbitrary prop_DeleteValid
sample_InsertPost = forAll arbitrary prop_InsertPost
sample_DeletePost = forAll arbitrary prop_DeletePost
sample_InsertModel = forAll arbitrary prop_InsertModel
sample_DeleteModel = forAll arbitrary prop_DeleteModel
sample_InsertInsert = forAll arbitrary prop_InsertInsert
sample_InsertDelete = forAll arbitrary prop_InsertDelete
sample_DeleteInsert = forAll arbitrary prop_DeleteInsert
sample_DeleteDelete = forAll arbitrary prop_DeleteDelete


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
  = [(("Correct", "prop_InsertValid"), sample_InsertValid),
     (("Correct", "prop_DeleteValid"), sample_DeleteValid),
     (("Correct", "prop_InsertPost"), sample_InsertPost),
     (("Correct", "prop_DeletePost"), sample_DeletePost),
     (("Correct", "prop_InsertModel"), sample_InsertModel),
     (("Correct", "prop_DeleteModel"), sample_DeleteModel),
     (("Correct", "prop_InsertInsert"), sample_InsertInsert),
     (("Correct", "prop_InsertDelete"), sample_InsertDelete),
     (("Correct", "prop_DeleteInsert"), sample_DeleteInsert),
     (("Correct", "prop_DeleteDelete"), sample_DeleteDelete)]