{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HaskellInDepth.CH1.PaolinoExercises.HitParadeSpec where

import HaskellInDepth.CH1.PaolinoExercises.HitParade
import Test.Hspec (describe, hspec, it, shouldBe, Spec)


spec :: Spec
spec = do
  describe "hits" do
    it "handles 1 votes" do
      shouldBe
        do hits 10 (addVote "route 66" noParade)
        do [Hit "route 66" 1]
    it "handles 2 votes for same song" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "route 66" noParade)
        do [Hit "route 66" 2]
    it "handles 2 votes for different song, lexicographically swapped" do
      shouldBe
        do hits 10 (addVote "message in a bottle" $ addVote "route 66" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 1]
    it "handles 2 votes for different song, lexicographically straight" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "message in a bottle" noParade)
        do [Hit "route 66" 1, Hit "message in a bottle" 1]
    it "handles 3 votes for 2 different songs" do
      shouldBe
        do hits 10 (addVote "route 66" $ addVote "route 66" $ addVote "message in a bottle" noParade)
        do [Hit "route 66" 2, Hit "message in a bottle" 1]
