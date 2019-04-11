{-# LANGUAGE RankNTypes #-}

module Parliament.Utils where

import Protolude
import Prelude (String)
import Data.Aeson
import Control.Lens

import qualified Data.ByteString.Lazy as B

readJson :: (FromJSON a) => String -> IO (Either String a)
readJson file = eitherDecode <$> B.readFile file

-- |
-- scale the given amt from oldBase to newBase
recalculate :: Int -> Int -> Int -> Int
recalculate oldBase newBase amt = 
  round $ fromIntegral amt / fromIntegral oldBase * fromIntegral newBase

-- |
-- rescale the list of integers to newBase in proportion
adjustList :: Int -> Lens' a Int -> [a] -> [a]
adjustList _ _ [] = []
adjustList newBase l xs@(hd : tl) = 
  let oldSum = sumOf (each.l) xs in
  if oldSum == newBase then xs 
  else 
    let newTail = tl & each.l %~ recalculate oldSum newBase
        newHead = hd & l .~ (newBase - sumOf (each.l) newTail) in
    newHead : newTail