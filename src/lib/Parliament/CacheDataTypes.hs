{-# LANGUAGE DuplicateRecordFields
           , TemplateHaskell #-}
module Parliament.CacheDataTypes where

import Protolude
import Prelude (String)
import Control.Lens

import Parliament.Common

{-
Data structures for better performance in computation.
-}

-- |
-- bill's contributed funds from districts by a district bill-specific fund OR category fund.
data BillStatus = BillStatus {
    _id :: !Int
  , _name :: !String
  , _reqdFund :: !Int
  , _contributions :: ![(DistrictName, Int)]
} deriving (Eq, Show)
makeLenses ''BillStatus

-- |
-- district's s default amount and max amount of a particular category.
data DistrictCatFund = DistrictCatFund {
    _districtNm :: !DistrictName
  , _amount :: !Int
  , _maxAmt :: !Int
} deriving (Eq, Show)
makeLenses ''DistrictCatFund

-- |
-- bills and district category funds of the same category
data BillDistrict = BillDistrict {
    _billStatuses :: ![BillStatus]
  , _catFunds :: ![DistrictCatFund]
} deriving (Eq, Show)
makeLenses ''BillDistrict

-- |
-- map category to bills and district category funds
type CategoryMap = Map Category BillDistrict

-- | 
-- category and bill funds of contributed districts of a bill
data DistrictBillFund = DistrictBillFund {
    _category :: !Category
  , _funds :: ![(DistrictName, Int)]
} deriving (Eq, Show)
makeLenses ''DistrictBillFund

-- | 
-- map billId to contributed district bill funds
-- It is used to apply all related bill funds to this bill.  It tells the contributed district its category to build DistrictMap.
type BillMap = Map BillId DistrictBillFund

-- | 
-- map (distictName, category) to bill fundings
-- It is used to check if all bill funds and category fund exceed the category max for that district and cap the amounts by proportion.
type DistrictMap = Map (DistrictName, Category) [(BillId, Int)] 