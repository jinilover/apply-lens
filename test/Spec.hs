import Test.Hspec
import qualified Parliament.StructureBuildupSpec as StructureBuildupSpec
import qualified Parliament.UtilsSpec as UtilsSpec
import qualified Parliament.FundDistributionSpec as FundDistributionSpec

import Protolude

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = StructureBuildupSpec.specs ++
        UtilsSpec.specs ++
        FundDistributionSpec.specs