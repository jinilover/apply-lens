module Parliament.Arbitraries where

import Data.String (String)

import Test.QuickCheck
import Test.QuickCheck.Instances.List

import Parliament.DataTypes

availFundLimit = 3000000

genAvailableFund :: Gen Int
genAvailableFund = choose (0, availFundLimit)

genInt = genAvailableFund

genInts :: Gen [Int]
genInts = choose (0, 200) >>= flip setLength genInt

genFund :: Int -> Gen Int
genFund n = (`div` n) <$> genAvailableFund

genAlphabet :: Gen Char
genAlphabet = elements (['a'..'z'] ++ ['A'..'Z'])

genName :: Gen String
genName = setLength 10 genAlphabet

genDistrict :: Gen District
genDistrict = District <$> genName
                       <*> genAvailableFund
                       <*> genCategoryFunds
                       <*> return []

    where genCategoryFunds = choose (0, 50) >>= 
                               (\n -> setLength n (genCategoryFund n))

          genCategoryFund n = CategoryFund <$> genName
                                           <*> genFund n
                                           <*> genFund n