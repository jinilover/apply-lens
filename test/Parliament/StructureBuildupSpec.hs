{-# LANGUAGE RankNTypes #-}

module Parliament.StructureBuildupSpec
  (specs) where

import Test.QuickCheck
import Test.Hspec

import Protolude
import Control.Lens
import qualified Data.Map as M

import Parliament.Arbitraries
import Parliament.TestUtils

import Parliament.DataTypes
import Parliament.CacheDataTypes
import Parliament.StructureBuildup
import Parliament.Utils

capDistrictFundingsCheck :: Spec
capDistrictFundingsCheck = 
    describe "StructureBuildupSpec capDistrictFundings check" $ 
    it "The funds are always <= the available fund" $
        forAll genDistrict $ \origD ->
        let newD@District{..} = capDistrictFundings origD
            underBudget l = sumOf (categoryFunds.each.l) newD <= _availableFund in
        and (underBudget <$> [defaultAmount, maxAmount])

capDistrictFundingsSpec :: Spec
capDistrictFundingsSpec =
    describe "StructureBuildupSpec capDistrictFundings spec" $ do
    it "only max amount exceeds available fund" $
        let d = District {_dName = "Palolene", _availableFund = 20000, 
                _categoryFunds = [
                    CategoryFund {_fCategory = "Defense", _defaultAmount = 1000, _maxAmount = 3000},
                    CategoryFund {_fCategory = "Welfare", _defaultAmount = 3000, _maxAmount = 3199},
                    CategoryFund {_fCategory = "Science", _defaultAmount = 5000, _maxAmount = 15000}], 
                _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]}
            expect = District {_dName = "Palolene", _availableFund = 20000, 
                    _categoryFunds = [ -- only _maxAmount columns are changed
                        CategoryFund {_fCategory = "Defense", _defaultAmount = 1000, _maxAmount = 2830},
                        CategoryFund {_fCategory = "Welfare", _defaultAmount = 3000, _maxAmount = 3018},
                        CategoryFund {_fCategory = "Science", _defaultAmount = 5000, _maxAmount = 14152}], 
                    _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]} in
        capDistrictFundings d `shouldBe` expect
    it "only default amount exceeds available fund" $
        let d = District {_dName = "Palolene", _availableFund = 200000, 
                _categoryFunds = [
                    CategoryFund {_fCategory = "Defense", _defaultAmount = 10000, _maxAmount = 3000},
                    CategoryFund {_fCategory = "Welfare", _defaultAmount = 300000, _maxAmount = 3199},
                    CategoryFund {_fCategory = "Science", _defaultAmount = 50000, _maxAmount = 15000}], 
                _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]}
            expect = District {_dName = "Palolene", _availableFund = 200000, 
                    _categoryFunds = [
                        CategoryFund {_fCategory = "Defense", _defaultAmount = 5555, _maxAmount = 3000},
                        CategoryFund {_fCategory = "Welfare", _defaultAmount = 166667, _maxAmount = 3199},
                        CategoryFund {_fCategory = "Science", _defaultAmount = 27778, _maxAmount = 15000}], 
                    _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]} in 
        capDistrictFundings d `shouldBe` expect
    it "both amounts exceed available fund" $
        let d = District {_dName = "Palolene", _availableFund = 20000, 
                _categoryFunds = [
                    CategoryFund {_fCategory = "Defense", _defaultAmount = 1000, _maxAmount = 3000},
                    CategoryFund {_fCategory = "Welfare", _defaultAmount = 30000, _maxAmount = 3199},
                    CategoryFund {_fCategory = "Science", _defaultAmount = 5000, _maxAmount = 15000}], 
                _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]} 
            expect = District {_dName = "Palolene", _availableFund = 20000, 
                    _categoryFunds = [
                        CategoryFund {_fCategory = "Defense", _defaultAmount = 555, _maxAmount = 2830},
                        CategoryFund {_fCategory = "Welfare", _defaultAmount = 16667, _maxAmount = 3018},
                        CategoryFund {_fCategory = "Science", _defaultAmount = 2778, _maxAmount = 14152}], 
                    _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]} in 
        capDistrictFundings d `shouldBe` expect
    it "no amount exceeds available fund" $
        let d = District {_dName = "Palolene", _availableFund = 200000, 
                _categoryFunds = [
                    CategoryFund {_fCategory = "Defense", _defaultAmount = 1000, _maxAmount = 3000},
                    CategoryFund {_fCategory = "Welfare", _defaultAmount = 3000, _maxAmount = 3199},
                    CategoryFund {_fCategory = "Science", _defaultAmount = 5000, _maxAmount = 15000}], 
                _billFunds = [BillFund {_billId = 4, _amount = 500},BillFund {_billId = 3, _amount = 7500}]} in
        capDistrictFundings d `shouldBe` d
    it "only bill funds exceeds available fund" $
        let d = District {_dName = "Palolene", _availableFund = 200000, 
                _categoryFunds = [
                    CategoryFund {_fCategory = "Defense", _defaultAmount = 1000, _maxAmount = 3000},
                    CategoryFund {_fCategory = "Welfare", _defaultAmount = 3000, _maxAmount = 3199},
                    CategoryFund {_fCategory = "Science", _defaultAmount = 5000, _maxAmount = 15000}], 
                _billFunds = [BillFund {_billId = 4, _amount = 500000},BillFund {_billId = 3, _amount = 7500}]} in
        capDistrictFundings d `shouldBe` d
    -- it "parliament1.json with sufficient available funding" $
    --   readJson (resrcFolder ++ "parliament1.json") >>= check1

capByAvailableFundSpec :: Spec
capByAvailableFundSpec = describe "StructureBuildupSpec capByAvailableFundSpec" $ do
    it "parliament1.json where all district category funds under available funds" $
        readJson (resrcFolder ++ "parliament1.json") >>= expectNoCap
    it "parliament2.json where all district category funds under available funds" $
        readJson (resrcFolder ++ "parliament2.json") >>= expectNoCap
    it "parliament3.json where all district category funds under available funds" $
        readJson (resrcFolder ++ "parliament3.json") >>= expectNoCap
    it "parliament4.json where 1st district has maxAmount exceed available fund" $
        readJson (resrcFolder ++ "parliament4.json") >>= expectFundExceeded
    
    where expectNoCap gazette = fmap capByAvailableFund gazette `shouldBe` gazette

          expectFundExceeded gazette = 
            let expGazette = (\g -> 
                               (\d -> g & districts.ix 0 .~ changeMaxAccounts d) <$> g ^? districts.ix 0
                             ) <$> gazette in 
                fmap (Just . capByAvailableFund) gazette `shouldBe` expGazette
        
          changeMaxAccounts d@District{..} = 
            let catFunds' = (\(c, v) -> c {_maxAmount = v}) 
                            <$> zip _categoryFunds [2830, 3018, 14152] in
                d {_categoryFunds = catFunds'}
          

groupFundsSpec :: Spec
groupFundsSpec = describe "StructureBuildupSpec groupFundsSpec" $ do
    it "single bill, only 1 district has fund for non-exist bill, 1 has fund capped by max" $
        readJson (resrcFolder ++ "parliament1.json") >>= check1
    it "bills of different categories, districts has no bill fund" $
        readJson (resrcFolder ++ "parliament2.json") >>= check2
    it "bills of different categories, have bill fund, has fund on non-exist bill, has fund capped by max, has 1 zero bill fund" $
        readJson (resrcFolder ++ "parliament3.json") >>= check3

    where check1 decoded = 
            let welfareBs = [BillStatus 2 "An Act to Construct Shelters for the Homeless" 40000 []]
                welfareFund = [DistrictCatFund "Lakos" 1000 3000, DistrictCatFund "Southern Palolene" 5000 10000, DistrictCatFund "Palolene" 3000 3199] 
                defenseFund = [DistrictCatFund "Lakos" 10000 30000, DistrictCatFund "Southern Palolene" 500 10000, DistrictCatFund "Palolene" 3000 3000]
                scienceFund = [DistrictCatFund "Lakos" 500 1000, DistrictCatFund "Southern Palolene" 2000 10000, DistrictCatFund "Palolene" 5000 15000] 
                catMap = M.fromList [
                    ("Welfare", BillDistrict welfareBs welfareFund)
                    , ("Defense", BillDistrict [] defenseFund)
                    , ("Science", BillDistrict [] scienceFund)] 
                billMap = M.fromList [ (2, DistrictBillFund "Welfare" []) ] in
            fmap groupFunds decoded `shouldBe` Right (catMap, billMap)

          check2 decoded = 
            let defenseBs = [
                    BillStatus 9 "An Act to Construct the Great Wall of Malodivo 9" 200000 []
                    , BillStatus 6 "An Act to Construct the Great Wall of Malodivo 6" 200000 []
                    , BillStatus 5 "An Act to Construct the Great Wall of Malodivo 5" 200000 []
                    , BillStatus 1 "An Act to Construct the Great Wall of Malodivo" 200000 []] 

                defenseFund = [DistrictCatFund "Lakos" 10000 30000, DistrictCatFund "Southern Palolene" 500 10000, DistrictCatFund "Palolene" 1000 3000]

                welfareBs = [
                    BillStatus 4 "An Act to Increase Retirement Benefits for Veterans" 90000 []
                    , BillStatus 2 "An Act to Construct Shelters for the Homeless" 40000 []]

                welfareFund = [DistrictCatFund "Lakos" 1000 3000, DistrictCatFund "Southern Palolene" 5000 10000, DistrictCatFund "Palolene" 3000 3199] 

                scienceBs = [
                    BillStatus 8 "An Act to Fund the Development of Longer-Lasting Paper 8" 14000 []
                    , BillStatus 7 "An Act to Fund the Development of Longer-Lasting Paper 7" 14000 []
                    , BillStatus 3 "An Act to Fund the Development of Longer-Lasting Paper" 14000 []]

                scienceFund = [DistrictCatFund "Lakos" 500 1000, DistrictCatFund "Southern Palolene" 2000 10000, DistrictCatFund "Palolene" 5000 15000] 

                catMap = M.fromList [
                    ("Welfare", BillDistrict welfareBs welfareFund)
                    , ("Defense", BillDistrict defenseBs defenseFund)
                    , ("Science", BillDistrict scienceBs scienceFund)]

                billMap = M.fromList [
                    (1, DistrictBillFund "Defense" [])
                    , (2, DistrictBillFund "Welfare" [])
                    , (3, DistrictBillFund "Science" [])
                    , (4, DistrictBillFund "Welfare" [])
                    , (5, DistrictBillFund "Defense" [])
                    , (6, DistrictBillFund "Defense" [])
                    , (7, DistrictBillFund "Science" [])
                    , (8, DistrictBillFund "Science" [])
                    , (9, DistrictBillFund "Defense" []) ] in
            fmap groupFunds decoded `shouldBe` Right (catMap, billMap)

          check3 decoded = 
            let defenseBs = [BillStatus 1 "An Act to Construct the Great Wall of Malodivo" 200000 []]

                defenseFund = [DistrictCatFund "Lakos" 2727 30000, DistrictCatFund "Southern Palolene" 500 10000, DistrictCatFund "Palolene" 1000 3000]

                welfareBs = [
                    BillStatus 4 "An Act to Increase Retirement Benefits for Veterans" 90000 []
                    , BillStatus 2 "An Act to Construct Shelters for the Homeless" 40000 [] ]

                welfareFund = [DistrictCatFund "Lakos" 1000 3000, DistrictCatFund "Southern Palolene" 5000 10000, DistrictCatFund "Palolene" 2742 3199]

                scienceBs = [BillStatus 3 "An Act to Fund the Development of Longer-Lasting Paper" 14000 []]

                scienceFund = [DistrictCatFund "Lakos" 500 1000, DistrictCatFund "Southern Palolene" 2000 10000, DistrictCatFund "Palolene" 5000 15000]

                catMap = M.fromList [
                    ("Welfare", BillDistrict welfareBs welfareFund)
                    , ("Defense", BillDistrict defenseBs defenseFund)
                    , ("Science", BillDistrict scienceBs scienceFund) ]
                    
                billMap = M.fromList [
                    (1, DistrictBillFund "Defense" [("Lakos", 27273)])
                    , (2, DistrictBillFund "Welfare" [])
                    , (3, DistrictBillFund "Science" [("Lakos", 0), ("Palolene", 7500)])
                    , (4, DistrictBillFund "Welfare" [("Palolene", 457)]) ] in
                    
            fmap groupFunds decoded `shouldBe` Right (catMap, billMap)
    
specs :: [Spec]
specs = [capDistrictFundingsCheck
       , capDistrictFundingsSpec
       , capByAvailableFundSpec
       , groupFundsSpec]