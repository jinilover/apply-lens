{-# LANGUAGE DeriveGeneric
           , DuplicateRecordFields
           , TemplateHaskell #-}

module Parliament.DataTypes where

import Data.String (String)
import Data.Aeson
import GHC.Generics
import Control.Lens

import Parliament.Common

{- for input json -}
data Bill = Bill {
    _id :: !BillId
  , _bName :: !String
  , _bCategory :: !String
  , _bAmount :: !Int
} deriving (Eq, Show, Generic)
instance FromJSON Bill
instance ToJSON Bill
makeLenses ''Bill

data CategoryFund = CategoryFund {
    _fCategory :: !String
  , _defaultAmount :: !Int
  , _maxAmount :: !Int
} deriving (Eq, Show, Generic)
makeLenses ''CategoryFund
instance FromJSON CategoryFund
instance ToJSON CategoryFund

data BillFund = BillFund {
    _billId :: !BillId
  , _amount :: !Int
} deriving (Eq, Show, Generic)
makeLenses ''BillFund
instance FromJSON BillFund
instance ToJSON BillFund

data District = District {
    _dName :: !String
  , _availableFund :: !Int
  , _categoryFunds :: ![CategoryFund]
  , _billFunds :: ![BillFund]
} deriving (Eq, Show, Generic)
makeLenses ''District
instance FromJSON District
instance ToJSON District

data Gazette = Gazette {
    _bills :: ![Bill]
  , _districts :: ![District]
} deriving (Eq, Show, Generic)
makeLenses ''Gazette
instance FromJSON Gazette
instance ToJSON Gazette

{- for output json -}
data Contribution = Contribution {
    _district :: !String
  , _contribute :: !Int
} deriving (Eq, Show, Generic)
instance FromJSON Contribution
instance ToJSON Contribution
makeLenses ''Contribution

data FinalBill = FinalBill {
    _fbName :: !String
  , _requiredFund :: !Int
  , _approvedFund :: !Int
  , _fullyFunded :: !Bool
  , _contributeFrom :: ![Contribution]
} deriving (Eq, Show, Generic)
instance FromJSON FinalBill
instance ToJSON FinalBill
makeLenses ''FinalBill

data DistrictFinance = DistrictFinance {
    _name :: !String
  , _origFund :: !Int
  , _expenses :: Int
  , _remainedFund :: !Int
} deriving (Eq, Show, Generic)
instance FromJSON DistrictFinance
instance ToJSON DistrictFinance
makeLenses ''DistrictFinance

data Report = Report {
    finalBills :: [FinalBill]
  , districtFinances :: [DistrictFinance]
} deriving (Show, Generic)
instance FromJSON Report
instance ToJSON Report