{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Parliament.Utils where

import Control.Monad.Except (liftEither)
import Data.String (String)
import Data.Aeson
import Control.Lens

import Parliament.Common

readJson
  :: (FromJSON a, MonadBSReader m, MonadError String m)
  => String -> m a
readJson = liftEither . eitherDecode <=< readByteString 

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