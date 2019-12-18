module Parliament.Common where

import Data.String (String)
import qualified Data.ByteString.Lazy as B

type BillId = Int

type Category = String

type DistrictName = String

-- refer to https://chrispenner.ca/posts/monadio-considered-harmful
class Monad m => MonadBSReader m where
  readByteString :: FilePath -> m B.ByteString

instance MonadBSReader IO where
  readByteString = B.readFile
    
instance MonadBSReader m => MonadBSReader (ExceptT e m) where
  readByteString = lift . readByteString