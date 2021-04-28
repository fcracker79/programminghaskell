{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module HaskellInDepth.CH1.PaolinoExercises.OrderedListSpec where


import Test.Hspec
import Data.FingerTree
import Protolude
import HaskellInDepth.CH1.PaolinoExercises.OrderedList(insert, remove)


newtype TestInt = TestInt Int deriving (Num, Show, Eq, Ord)

instance Measured (Last TestInt) TestInt where
  measure = Last . Just 


spec :: Spec
spec = do
  describe "insert" do
    it "add an element" do
      toList (insert 1 mempty) `shouldBe` [1 :: TestInt]
    it "add 2 elements" do
      shouldBe
        do toList $ insert 2 $ insert 1 mempty
        do [1, 2 :: TestInt]
    it "add 2 elements reversed sorting" do
      shouldBe
        do toList $ insert 1 $ insert 2 mempty
        do [1, 2 :: TestInt]
    it "add 3 elements with repetition" do
      shouldBe
        do toList $ insert 2 $ insert 1 $ insert 2 mempty
        do [1, 2, 2 :: TestInt]
    it "add 4 elements with repetition" do
      shouldBe
        do toList $ insert 0 $ insert 2 $ insert 1 $ insert 2 mempty
        do [0, 1, 2, 2 :: TestInt]
  describe "remove" do
    it "delete an element" do
      toList (remove 1 $ insert 1 mempty) `shouldBe` ([] :: [TestInt])
    it "delete 1 element" do
      shouldBe
        do toList $ remove 1 $ insert 2 $ insert 1 mempty
        do [2 :: TestInt]
    it "delete 1 element reversed sorting" do
      shouldBe
        do toList $ remove 2 $ insert 1 $ insert 2 mempty
        do [1 :: TestInt]
    it "remove 2 elements with repetition" do
      shouldBe
        do toList $ remove 1 $ remove 2 $ insert 2 $ insert 1 $ insert 2 mempty
        do [2 :: TestInt]
    it "remove 3 elements with repetition" do
      shouldBe
        do toList $ remove 2 $ insert 0 $ insert 2 $ remove 2 $ insert 1 $ insert 2 mempty
        do [0, 1 :: TestInt]

