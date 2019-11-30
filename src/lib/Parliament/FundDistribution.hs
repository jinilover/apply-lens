{-# LANGUAGE GADTs #-}
module Parliament.FundDistribution where

import Data.List (partition)
import Control.Lens
import Data.String (String)
import qualified Data.Map as M

import Parliament.DataTypes
import Parliament.CacheDataTypes
import Parliament.Utils

{-
Fund approval computation.

* Apply bill funds and category fund to each bill accordingly

* Refund the funds to districts if the bill has extra fund approved

* Generate the final bill and fund remained in each districts

-}

-- |
-- Apply bill funds and category funds to each bill accordingly in the following sequence
-- 
-- * in each category, make use of BillMap to apply any available bill funds to BillStatus.  
-- * if the bill is fully funded, FinalBill will be created, its BillStatus will be removed such that no subsequent category fund will be applied.
-- * apply district' category funds to only those BillStatus(es) which weren't applied by the same district's bill funded.
-- * if there are multiple BillStatus(es), the category fund will be applied to them in proportion.
approveFund :: (CategoryMap, BillMap) -> [FinalBill]
approveFund (catMap, billMap) = join . map forEachCategory . M.elems $ catMap
  where forEachCategory BillDistrict{..} = 
          -- state monad where the state is ([BillStatus], [FinalBill]) 
          let s = procBillFunds >> traverse_ procCatFunds _catFunds 
              (remainedBs, fbs) = execState s (_billStatuses, []) in 
          map createFinalBill remainedBs ++ fbs
        
        procBillFunds = 
          withState (\(billStatuses, finalBills) -> 
            let (finishedBs, remainBs) = partition isBillFinished . map applyBillFunds $ billStatuses in
            (remainBs, finalBills ++ map createFinalBill finishedBs)
          ) get

        isBillFinished BillStatus{..} = (sum . map snd $ _contributions) >= _reqdFund 

        applyBillFunds b@BillStatus{..} = 
          maybe b (\DistrictBillFund{..} -> b & contributions %~ (++ _funds)) $ M.lookup _id billMap

        procCatFunds (DistrictCatFund dName dFund _) = 
          withState (\(bills, fbs) ->
            -- ignore those bills already covered by this district's bill specific funding before
            let (ignoredBs, targetBs) = partition (any ((dName ==) . fst) . _contributions) bills 
                reqdFunds = map _reqdFund targetBs
                approvals = adjustList dFund identity reqdFunds 
                updatedBs = [b & contributions %~ ((dName, fund):) | (b,fund) <- zip targetBs approvals] in
            (ignoredBs ++ updatedBs, fbs)
          ) get

createFinalBill :: BillStatus -> FinalBill
createFinalBill BillStatus{..} =
  FinalBill {
      _fbName = _name
    , _requiredFund = _reqdFund
    , _approvedFund = sum . map snd $ _contributions
    , _fullyFunded = False
    , _contributeFrom = fmap (uncurry Contribution) _contributions
  }

districtFinance :: District -> DistrictFinance
districtFinance (District dn availFund _ _) =
  DistrictFinance {
      _name = dn
    , _origFund = availFund
    , _expenses = 0
    , _remainedFund = availFund
  }

-- |
-- Scan the final bills and, if any of them is extra funded, it will be refunded to the districts in proportion.  It also generate the funds of each district after apply funds to the bill.
chargeDistricts :: [FinalBill] -> [District] -> ([FinalBill], [DistrictFinance])
chargeDistricts fbs ds = 
  let districtFinanceM = M.fromList . map (\d@District{..} -> (_dName, districtFinance d)) $ ds 
      report = foldr capFullyFundedBill ([], districtFinanceM) fbs in
  second (fmap calculateRemainFund . M.elems) report
  where capFullyFundedBill fb (fbs, dM) = 
          let fb' = refund fb
              dM' = execState (traverse_ refundDistrict $ _contributeFrom fb') dM in
          (fb' : fbs, dM')

        refundDistrict (Contribution dName amt) = 
          withState (
            ix dName %~ (expenses %~ (amt+))
          ) get

        calculateRemainFund d@DistrictFinance{..} = d {_remainedFund = _remainedFund - _expenses}


refund :: FinalBill -> FinalBill
refund b@FinalBill{..} 
  | _approvedFund < _requiredFund = b 
  | _approvedFund == _requiredFund = b {_fullyFunded = True}
  | otherwise = 
    b & (contributeFrom %~ adjustList _requiredFund contribute)
      & (approvedFund .~ _requiredFund)
      & (fullyFunded .~ True)