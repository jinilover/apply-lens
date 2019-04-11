# apply-lens
There are numerous articles explaining what are lenses and why they are used.  This repo illustrates some scenarios of using `Traversal` and using lens to update an existing `Map` to avoid tedious code.

## Update `Map`

A common way to update an existing entry is
```
maybe m (M.insert k f) $ M.lookup k m
-- m :: M.Map k v
-- f :: v -> v
```

By using lens
```
import Control.Lens
ix k %~ f $ m
-- "%~" is infix operator of function "over"
```

## Using `each` for `Traversal`
Consider the following types:
```
data CategoryFund = CategoryFund {
    _fCategory :: !String
  , _defaultAmount :: !Int
  , _maxAmount :: !Int
}

data District = District {
    _dName :: !String
  , _availableFund :: !Int
  , _categoryFunds :: ![CategoryFund]
}

data Contribution = Contribution {
    _district :: !String
  , _contribute :: !Int
}

data FinalBill = FinalBill {
    _fbName :: !String
  , _requiredFund :: !Int
  , _approvedFund :: !Int
  , _fullyFunded :: !Bool
  , _contributeFrom :: ![Contribution]
}
```

### Requirement:
* In `District`, if summation of `_defaultAmount` of `[CategoryFund]` is greater than `_availableFund`, all `_defaultAmount` values should be capped by `_availableFund` by proportion according to the original summation of `_defaultAmount`.
* A similar requirement on `_maxAmount` of `[CategoryFund]`.
* A similar requirement on `FinalBill` if summation of `_contribute` of `[Contribution]` is greater than `_requiredFund`.

### Problem:
* The code of calculating the new values are the same.
* The new values need to be applied on the list of data structures.  A typical usage of lens.

### Solution:



## References
* https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
