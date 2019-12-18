{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Parliament.Utils where

import Control.Monad.Except (liftEither)
import Data.String (String)
import Data.Aeson
import Control.Lens

import qualified Data.ByteString.Lazy as B

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

-- refer to https://chrispenner.ca/posts/monadio-considered-harmful
class Monad m => MonadBSReader m where
  readByteString :: FilePath -> m B.ByteString

instance MonadBSReader IO where
  readByteString = B.readFile

instance MonadBSReader m => MonadBSReader (ExceptT e m) where
  readByteString = lift . readByteString