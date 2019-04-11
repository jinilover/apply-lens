{-# LANGUAGE RankNTypes
           , FlexibleContexts
           , GADTs #-}

module Parliament.StructureBuildup where

import Protolude
import Control.Lens
import qualified Data.Map as M
import qualified Prelude as P (id)

import Parliament.Utils
import Parliament.DataTypes
import Parliament.Common
import Parliament.CacheDataTypes

{-
Shape the data in an appropriate data structure suitable for fund distribution computation.  The work includes

* The category fundings are capped by the district available fund by original proportion

* Fundings and bill fundings of the same category are capped by the max amount by original proportion

* Data are put in a data structure convenient and well performed for computation.
-}

-- |
-- Cap the district's category default amounts and max amounts if they exceed its available fund
capDistrictFundings :: District -> District
capDistrictFundings d@District{..} = d & adjustFunds maxAmount 
                                       & adjustFunds defaultAmount
    where adjustFunds :: Lens' CategoryFund Int -> District -> District
          adjustFunds l d@District{..} = 
            let planSum = sumOf (categoryFunds.each.l) d in
            if _availableFund >= planSum
            then d
            else categoryFunds %~ adjustList _availableFund l $ d

capByAvailableFund :: Gazette -> Gazette
capByAvailableFund = districts %~ map capDistrictFundings

-- |
-- put the data in appropriate data structures for subsequent fund approval computation.
-- Data are put in the following sequence to maximise the efficiency.
-- 
-- * process the gazette's bill data, add the bill data to BillMap
-- * add the bill data to CategoryMap
-- * process the gazette's district data, add the district's bill fund data to DistrictMap
-- * process the district's category fund data, cap the category (if exceeds max) fund amount with the bill fund data from DistrictMap
-- * add the capped bill fund data to BillMap
-- * add the capped category fund data to CategoryMap
-- * omits DistrictMap because it's temp cache for capping the district category funds only
-- * CategoryMap and BillMap are ready for fund approval
groupFunds :: Gazette -> (CategoryMap, BillMap)
groupFunds (Gazette bs ds) = 
    -- state monad where the state is (CategoryMap, BillMap, DistrictMap)
    let s = traverse_ procBill bs >> 
            traverse_ procDistrict ds 
        (catMap, billMap, _) = execState s (M.empty, M.empty, M.empty) in 
    (catMap, billMap)
  where procBill (Bill id name cat amt) = 
          do
            let b = BillStatus id name amt [] in
              updateCatMap cat (BillDistrict [b] []) (billStatuses %~ (b :))  
            (catMap, billMap, districtMap) <- get
            let billMap' = M.insert id (DistrictBillFund cat []) billMap in
                put (catMap, billMap', districtMap)

        procDistrict (District name _ catFunds billFunds) = 
          traverse_ (addBillFund name) billFunds >> 
          traverse_ (addCatFund name) catFunds

        -- if a district covers a non-exist bill,
        -- it will ignore the bill fund,
        -- o.w. it will add the bill fund to DistrictMap for subsequent per-category capping.
        addBillFund dName BillFund{..} = 
          withState (\(catMap, billMap, districtMap) ->
            let districtMap' = 
                  maybe districtMap (existBill dName _amount _billId districtMap) 
                  $ M.lookup _billId billMap in
            (catMap, billMap, districtMap')
          ) get
            
        existBill dName amt bId districtMap dbf@DistrictBillFund{..} = 
          let key = (dName, _category)
              billAmt = (bId, amt) in
          if M.member key districtMap
            then ix key %~ (billAmt :) $ districtMap
            else M.insert key [billAmt] districtMap
    
        addCatFund dName (CategoryFund cat defaultAmt maxAmt) = 
          do
            catFund <- capCatFund dName cat defaultAmt maxAmt 
            updateCatMap cat (BillDistrict [] [catFund]) $ catFunds %~ (catFund :)

        -- if the district has bill fund(s) of that category,
        -- it will cap both category fund and the bill funds by proportion,
        -- o.w. it will cap the category fund only.
        capCatFund dName cat defaultAmt maxAmt = 
          state (\(catMap, billMap, districtMap) ->
            let key = (dName, cat) 
                (defaultAmt', billMap') = 
                  maybe 
                    (if defaultAmt > maxAmt then maxAmt else defaultAmt, billMap) 
                    (existDistrict dName billMap defaultAmt maxAmt)
                    $ M.lookup key districtMap in
            (DistrictCatFund dName defaultAmt' maxAmt, (catMap, billMap', districtMap))
          )
        
        existDistrict dName billMap defaultAmt maxAmt pairs = 
          let billFunds = map snd pairs
              total = defaultAmt + sum billFunds
              xs = defaultAmt : billFunds
              -- cap the funds if they are over max
              (defaultAmt' : billFunds') = 
                if total > maxAmt then adjustList maxAmt P.id xs else xs
              billMap' = foldl (\billMap ((bId, _), amt) ->
                           ix bId %~ (funds %~ ((dName, amt):)) $ billMap -- https://hackage.haskell.org/package/lens-4.17/docs/Data-Map-Lens.html
                         ) billMap $ zip pairs billFunds' in
          (defaultAmt', billMap')

        updateCatMap cat bd f = 
          withState (\(catMap, billMap, districtMap) -> 
            let bd' = maybe bd f (M.lookup cat catMap) in 
            (M.insert cat bd' catMap, billMap, districtMap)
          ) get