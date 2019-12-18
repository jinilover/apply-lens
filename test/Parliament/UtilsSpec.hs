module Parliament.UtilsSpec
  (specs) where

import Control.Lens

import Test.QuickCheck
import Test.Hspec

import Parliament.Arbitraries
import Parliament.Utils

adjustListSpec :: Spec
adjustListSpec = describe "UtilsSpec adjustListSpec" $ do
  it "test [10000, 30000, 90000], newbase as 20000" $
    let xs = [10000, 30000, 90000] in
    adjustList 20000 identity xs `shouldBe` [1539, 4615, 13846]
  it "test [30000, 90000], newbase as 20000" $
    let xs = [30000, 90000] in
    adjustList 30000 identity xs `shouldBe` [7500 ,22500]
  it "test [9000, 30000, 90000], newbase as 8000" $
    let xs = [9000, 30000, 90000] in
    adjustList 8000 identity xs `shouldBe` [559, 1860, 5581]
  it "test [9000, 10000, 30000, 90000], newbase as 40000" $
    let xs = [9000, 10000, 30000, 90000] in
    adjustList 40000 identity xs `shouldBe` [2590, 2878, 8633, 25899]
  it "test [900, 1000, 3000, 9000], newbase as 40000" $
    let xs = [900, 1000, 3000, 9000] in
    adjustList 40000 identity xs `shouldBe` [2590, 2878, 8633, 25899]
  it "test [900], newbase as 40000" $
    let xs = [900] in
    adjustList 40000 identity xs `shouldBe` [40000]
  it "test [90000], newbase as 40000" $
    let xs = [90000] in
    adjustList 40000 identity xs `shouldBe` [40000]

adjustListCheck :: Spec
adjustListCheck =
    describe "UtilsSpec adjustListCheck" $
      it "The ints are adjusted and always sum up to the required value" $
        forAll genInt $ \newbase ->
          forAll genInts $ \xs ->
            let xs' = adjustList newbase identity xs in
            xs == [] && xs == xs' ||
            sum xs == newbase && xs' == xs ||
            sum xs' == newbase && xs' /= xs 
    

specs :: [Spec]
specs = [adjustListSpec, adjustListCheck]
    