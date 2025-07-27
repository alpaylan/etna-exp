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
import Impl (Tree (..))
import Spec (BST, Key, Val)

import System.Environment (getArgs)
import Control.DeepSeq
import Control.Monad (liftM2, liftM3, liftM4, liftM5)
import Test.QuickCheck hiding (sample)

type Sample i = i -> Bool 

instance NFData Key where
  rnf k = k `seq` ()

instance NFData Val where
  rnf v = v `seq` ()

instance (NFData k, NFData v) => NFData (Tree k v) where
  rnf E = ()
  rnf (T l k v r) = rnf k `seq` rnf v `seq` rnf l `seq` rnf r


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

prop_InsertValid :: Sample (Arg3 BST Key Val)
prop_InsertValid (Arg3 t k v) =
  deepseq (t,k,v) True

prop_DeleteValid :: Sample (Arg2 BST Key)
prop_DeleteValid (Arg2 t k) =
  deepseq (t,k) True

prop_UnionValid :: Sample (Arg2 BST BST)
prop_UnionValid (Arg2 t1 t2) =
  deepseq (t1, t2) True

----------

-- Postcondition properties.

prop_InsertPost :: Sample (Arg4 BST Key Key Val)
prop_InsertPost (Arg4 t k k' v) =
  deepseq (t, k, k', v) True

prop_DeletePost :: Sample (Arg3 BST Key Key)
prop_DeletePost (Arg3 t k k') =
  deepseq (t, k, k') True

prop_UnionPost :: Sample (Arg3 BST BST Key)
prop_UnionPost (Arg3 t t' k) =
  deepseq (t, t', k) True

----------

-- Model-based properties.

prop_InsertModel :: Sample (Arg3 BST Key Val)
prop_InsertModel (Arg3 t k v) =
  deepseq (t, k, v) True

prop_DeleteModel :: Sample (Arg2 BST Key)
prop_DeleteModel (Arg2 t k) =
  deepseq (t, k) True

prop_UnionModel :: Sample (Arg2 BST BST)
prop_UnionModel (Arg2 t t') =
  deepseq (t, t') True

----------

-- Metamorphic properties.

prop_InsertInsert :: Sample (Arg5 BST Key Key Val Val)
prop_InsertInsert (Arg5 t k k' v v') =
  deepseq (t, k, k', v, v') True

prop_InsertDelete :: Sample (Arg4 BST Key Key Val)
prop_InsertDelete (Arg4 t k k' v) =
  deepseq (t, k, k', v) True

prop_InsertUnion :: Sample (Arg4 BST BST Key Val)
prop_InsertUnion (Arg4 t t' k v) =
  deepseq (t, t', k, v) True

prop_DeleteInsert :: Sample (Arg4 BST Key Key Val)
prop_DeleteInsert (Arg4 t k k' v') =
  deepseq (t, k, k', v') True

prop_DeleteDelete :: Sample (Arg3 BST Key Key)
prop_DeleteDelete (Arg3 t k k') =
  deepseq (t, k, k') True

prop_DeleteUnion :: Sample (Arg3 BST BST Key)
prop_DeleteUnion (Arg3 t t' k) =
  deepseq (t, t', k) True

prop_UnionDeleteInsert :: Sample (Arg4 BST BST Key Val)
prop_UnionDeleteInsert (Arg4 t t' k v) =
  deepseq (t, t', k, v) True

prop_UnionUnionIdem :: Sample BST
prop_UnionUnionIdem t =
  deepseq t True

prop_UnionUnionAssoc :: Sample (Arg3 BST BST BST)
prop_UnionUnionAssoc (Arg3 t1 t2 t3) =
  deepseq (t1, t2, t3) True


sample_InsertValid = forAll arbitrary prop_InsertValid
sample_DeleteValid = forAll arbitrary prop_DeleteValid
sample_UnionValid = forAll arbitrary prop_UnionValid
sample_InsertPost = forAll arbitrary prop_InsertPost
sample_DeletePost = forAll arbitrary prop_DeletePost
sample_UnionPost = forAll arbitrary prop_UnionPost
sample_InsertModel = forAll arbitrary prop_InsertModel
sample_DeleteModel = forAll arbitrary prop_DeleteModel
sample_UnionModel = forAll arbitrary prop_UnionModel
sample_InsertInsert = forAll arbitrary prop_InsertInsert
sample_InsertDelete = forAll arbitrary prop_InsertDelete
sample_InsertUnion = forAll arbitrary prop_InsertUnion
sample_DeleteInsert = forAll arbitrary prop_DeleteInsert
sample_DeleteDelete = forAll arbitrary prop_DeleteDelete
sample_DeleteUnion = forAll arbitrary prop_DeleteUnion
sample_UnionDeleteInsert = forAll arbitrary prop_UnionDeleteInsert
sample_UnionUnionIdem = forAll arbitrary prop_UnionUnionIdem
sample_UnionUnionAssoc = forAll arbitrary prop_UnionUnionAssoc


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
     (("Correct", "prop_UnionValid"), sample_UnionValid),
     (("Correct", "prop_InsertPost"), sample_InsertPost),
     (("Correct", "prop_DeletePost"), sample_DeletePost),
     (("Correct", "prop_UnionPost"), sample_UnionPost),
     (("Correct", "prop_InsertModel"), sample_InsertModel),
     (("Correct", "prop_DeleteModel"), sample_DeleteModel),
     (("Correct", "prop_UnionModel"), sample_UnionModel),
     (("Correct", "prop_InsertInsert"), sample_InsertInsert),
     (("Correct", "prop_InsertDelete"), sample_InsertDelete),
     (("Correct", "prop_InsertUnion"), sample_InsertUnion),
     (("Correct", "prop_DeleteInsert"), sample_DeleteInsert),
     (("Correct", "prop_DeleteDelete"), sample_DeleteDelete),
     (("Correct", "prop_DeleteUnion"), sample_DeleteUnion),
     (("Correct", "prop_UnionDeleteInsert"), sample_UnionDeleteInsert),
     (("Correct", "prop_UnionUnionIdem"), sample_UnionUnionIdem),
     (("Correct", "prop_UnionUnionAssoc"), sample_UnionUnionAssoc)]