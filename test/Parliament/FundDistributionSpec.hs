module Parliament.FundDistributionSpec 
  (specs) where

import Protolude

import Test.QuickCheck
import Test.Hspec

import Parliament.TestUtils

import Parliament.StructureBuildup
import Parliament.Utils
import Parliament.DataTypes
import Parliament.FundDistribution

approveFundSpec :: Spec
approveFundSpec = 
  describe "FundDistributionSpec approveFundSpec" $ do
    it "parliament3.json, has 1 zero bill fund" $
      readJson (resrcFolder ++ "parliament3.json") >>= check3
    it "parliament5.json, similar to parliament3.json, but Lakos will fund the paper bill via welfare fund" $
      readJson (resrcFolder ++ "parliament5.json") >>= check5
    it "parliament6.json, similar to parliament3.json, but 1 more welfare bill fund is added to Palelone" $
      readJson (resrcFolder ++ "parliament6.json") >>= check6
    it "parliament7.json, similar to parliament3.json, but the paper bill can be covered by Palelone's bill fund only" $
      readJson (resrcFolder ++ "parliament7.json") >>= check7
    it "parliament8.json, similar to parliament3.json, but the paper bill can be fully funded by different funds" $
      readJson (resrcFolder ++ "parliament8.json") >>= check8

    where check3 decoded = 
            fmap approvalFunc decoded `shouldBe` Right parliament3_fbs

          check5 decoded = 
            let expFbs = [ 
                    FinalBill {
                      _fbName = "An Act to Construct the Great Wall of Malodivo"
                    , _requiredFund = 200000
                    , _approvedFund = 28773
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 1000, Contribution "Southern Palolene" 500, Contribution "Lakos" 27273]}
                  , FinalBill {
                      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                    , _requiredFund = 14000
                    , _approvedFund = 10000 -- this time Lakos fund this paper via science fund becoz it removed zero fund on this bill
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 2000, Contribution "Lakos" 500, Contribution "Palolene" 7500]}
                  , FinalBill {
                      _fbName = "An Act to Increase Retirement Benefits for Veterans"
                    , _requiredFund = 90000
                    , _approvedFund = 4611
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 3462, Contribution "Lakos" 692, Contribution "Palolene" 457]}
                  , FinalBill {
                      _fbName = "An Act to Construct Shelters for the Homeless"
                    , _requiredFund = 40000
                    , _approvedFund = 4588
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 2742, Contribution "Southern Palolene" 1538, Contribution "Lakos" 308]} ] in
            fmap approvalFunc decoded `shouldBe` Right expFbs

          check6 decoded = 
            let expFbs = [
                    FinalBill {
                      _fbName = "An Act to Construct the Great Wall of Malodivo"
                    , _requiredFund = 200000
                    , _approvedFund = 28773
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 1000, Contribution "Southern Palolene" 500, Contribution "Lakos" 27273]}
                  , FinalBill {
                      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                    , _requiredFund = 14000
                    , _approvedFund = 9500
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 2000, Contribution "Lakos" 0,Contribution "Palolene" 7500]}
                  , FinalBill {
                      _fbName = "An Act to Increase Retirement Benefits for Veterans"
                    , _requiredFund = 90000
                    , _approvedFund = 4554 -- changed by Palolene's bill funding
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 400]}
                  , FinalBill {
                      _fbName = "An Act to Construct Shelters for the Homeless"
                    , _requiredFund = 40000
                    , _approvedFund = 2246 -- changed by Palolene's bill funding
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 1538,Contribution "Lakos" 308,Contribution "Palolene" 400]} ] in
            fmap approvalFunc decoded `shouldBe` Right expFbs

          check7 decoded = 
            let expFbs = [
                    FinalBill {
                      _fbName = "An Act to Construct the Great Wall of Malodivo"
                    , _requiredFund = 200000
                    , _approvedFund = 28773
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 1000,Contribution "Southern Palolene" 500,Contribution "Lakos" 27273]}
                  , FinalBill {
                      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                    , _requiredFund = 7500
                    , _approvedFund = 7500
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Lakos" 0,Contribution "Palolene" 7500]}
                  , FinalBill {
                      _fbName = "An Act to Increase Retirement Benefits for Veterans"
                    , _requiredFund = 90000
                    , _approvedFund = 4611
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 457]}
                  , FinalBill {
                      _fbName = "An Act to Construct Shelters for the Homeless"
                    , _requiredFund = 40000
                    , _approvedFund = 4588
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 2742,Contribution "Southern Palolene" 1538,Contribution "Lakos" 308]} ] in
            fmap approvalFunc decoded `shouldBe` Right expFbs

          check8 decoded = 
              let expFbs = [
                      FinalBill {
                        _fbName = "An Act to Construct the Great Wall of Malodivo"
                      , _requiredFund = 200000
                      , _approvedFund = 28773
                      , _fullyFunded = False
                      , _contributeFrom = [Contribution "Palolene" 1000,Contribution "Southern Palolene" 500,Contribution "Lakos" 27273]}
                    , FinalBill {
                        _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                      , _requiredFund = 9000
                      , _approvedFund = 9500
                      , _fullyFunded = False
                      , _contributeFrom = [Contribution "Southern Palolene" 2000,Contribution "Lakos" 0,Contribution "Palolene" 7500]}
                    , FinalBill {
                        _fbName = "An Act to Increase Retirement Benefits for Veterans"
                      , _requiredFund = 90000
                      , _approvedFund = 4611
                      , _fullyFunded = False
                      , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 457]}
                    , FinalBill {
                        _fbName = "An Act to Construct Shelters for the Homeless"
                      , _requiredFund = 40000
                      , _approvedFund = 4588
                      , _fullyFunded = False
                      , _contributeFrom = [Contribution "Palolene" 2742,Contribution "Southern Palolene" 1538,Contribution "Lakos" 308]} ] in
              fmap approvalFunc decoded `shouldBe` Right expFbs  

districtFinanceSpec :: Spec
districtFinanceSpec = 
  describe "FundDistributionSpec districtFinanceSpec" $ do
    it "parliament3.json, Lakos doesn't want to fund the long lasting paper bill" $
      readJson (resrcFolder ++ "parliament3.json") >>= check3
    it "parliament7.json, similar to the test in approveFundSpec, but 1 final bill will be marked fully funded" $
      readJson (resrcFolder ++ "parliament7.json") >>= check7
    it "parliament8.json, similar to the test in approveFundSpec, but it will be marked and refund the extra" $
      readJson (resrcFolder ++ "parliament8.json") >>= check8

    where check3 decoded = 
            let expDistrictFinance = [
                  DistrictFinance {_name = "Lakos", _origFund = 400000, _expenses = 28273, _remainedFund = 371727},
                  DistrictFinance {_name = "Palolene", _origFund = 200000, _expenses = 11699, _remainedFund = 188301},
                  DistrictFinance {_name = "Southern Palolene", _origFund = 150000, _expenses = 7500, _remainedFund = 142500}] in
            fmap (\g@Gazette{..} -> chargeDistrictsFunc g _districts) decoded
            `shouldBe` Right (parliament3_fbs, expDistrictFinance)

          check7 decoded = 
            let expFbs = [
                    FinalBill {
                      _fbName = "An Act to Construct the Great Wall of Malodivo"
                    , _requiredFund = 200000
                    , _approvedFund = 28773
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 1000,Contribution "Southern Palolene" 500,Contribution "Lakos" 27273]}
                  , FinalBill {
                      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                    , _requiredFund = 7500
                    , _approvedFund = 7500
                    , _fullyFunded = True -- marked by the chargeDistrict fund
                    , _contributeFrom = [Contribution "Lakos" 0,Contribution "Palolene" 7500]}
                  , FinalBill {
                      _fbName = "An Act to Increase Retirement Benefits for Veterans"
                    , _requiredFund = 90000
                    , _approvedFund = 4611
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 457]}
                  , FinalBill {
                      _fbName = "An Act to Construct Shelters for the Homeless"
                    , _requiredFund = 40000
                    , _approvedFund = 4588
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 2742,Contribution "Southern Palolene" 1538,Contribution "Lakos" 308]} ] 
                expDistrictFinance = [
                  DistrictFinance {_name = "Lakos", _origFund = 400000, _expenses = 28273, _remainedFund = 371727},
                  DistrictFinance {_name = "Palolene", _origFund = 200000, _expenses = 11699, _remainedFund = 188301},
                  DistrictFinance {_name = "Southern Palolene", _origFund = 150000, _expenses = 5500, _remainedFund = 144500}] in
            fmap (\g@Gazette{..} -> chargeDistrictsFunc g _districts) decoded
            `shouldBe` Right (expFbs, expDistrictFinance)
          
          check8 decoded = 
            let expFbs = [
                    FinalBill {
                      _fbName = "An Act to Construct the Great Wall of Malodivo"
                    , _requiredFund = 200000
                    , _approvedFund = 28773
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 1000,Contribution "Southern Palolene" 500,Contribution "Lakos" 27273]}
                  , FinalBill {
                      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
                    , _requiredFund = 9000
                    , _approvedFund = 9000
                    , _fullyFunded = True
                    , _contributeFrom = [Contribution "Southern Palolene" 1895,Contribution "Lakos" 0,Contribution "Palolene" 7105]}
                  , FinalBill {
                      _fbName = "An Act to Increase Retirement Benefits for Veterans"
                    , _requiredFund = 90000
                    , _approvedFund = 4611
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 457]}
                  , FinalBill {
                      _fbName = "An Act to Construct Shelters for the Homeless"
                    , _requiredFund = 40000
                    , _approvedFund = 4588
                    , _fullyFunded = False
                    , _contributeFrom = [Contribution "Palolene" 2742,Contribution "Southern Palolene" 1538,Contribution "Lakos" 308]} ] 
                expDistrictFinance = [
                  DistrictFinance {_name = "Lakos", _origFund = 400000, _expenses = 28273, _remainedFund = 371727},
                  DistrictFinance {_name = "Palolene", _origFund = 200000, _expenses = 11304, _remainedFund = 188696},
                  DistrictFinance {_name = "Southern Palolene", _origFund = 150000, _expenses = 7395, _remainedFund = 142605}]  in
            fmap (\g@Gazette{..} -> chargeDistrictsFunc g _districts) decoded
            `shouldBe` Right (expFbs, expDistrictFinance)

approvalFunc :: Gazette -> [FinalBill]
approvalFunc = approveFund . groupFunds . capByAvailableFund

chargeDistrictsFunc :: Gazette -> [District] -> ([FinalBill], [DistrictFinance])
chargeDistrictsFunc = chargeDistricts . approvalFunc

-- this sample data is shared by multiple test cases
-- Lakos doesn't want to funding the 2nd bill even it has science funding
-- Lakos and S. Palolene welfare funding is distributed by proportion to the 2 bills
-- Palolene welfare funding is capped by max amount
parliament3_fbs = [
    FinalBill {
      _fbName = "An Act to Construct the Great Wall of Malodivo"
    , _requiredFund = 200000
    , _approvedFund = 28773
    , _fullyFunded = False
    , _contributeFrom = [Contribution "Palolene" 1000,Contribution "Southern Palolene" 500,Contribution "Lakos" 27273]}
  , FinalBill {
      _fbName = "An Act to Fund the Development of Longer-Lasting Paper"
    , _requiredFund = 14000
    , _approvedFund = 9500
    , _fullyFunded = False
    , _contributeFrom = [Contribution "Southern Palolene" 2000,Contribution "Lakos" 0,Contribution "Palolene" 7500]}
  , FinalBill {
      _fbName = "An Act to Increase Retirement Benefits for Veterans"
    , _requiredFund = 90000
    , _approvedFund = 4611
    , _fullyFunded = False
    , _contributeFrom = [Contribution "Southern Palolene" 3462,Contribution "Lakos" 692,Contribution "Palolene" 457]}
  , FinalBill {
      _fbName = "An Act to Construct Shelters for the Homeless"
    , _requiredFund = 40000
    , _approvedFund = 4588
    , _fullyFunded = False
    , _contributeFrom = [Contribution "Palolene" 2742,Contribution "Southern Palolene" 1538,Contribution "Lakos" 308]} ] 

specs :: [Spec]
specs = [approveFundSpec, districtFinanceSpec]