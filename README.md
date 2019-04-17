# apply-lens
There are numerous articles explaining what are lenses and why they are used.  This repo shares my experience of using lens in the following scenarios:
* Update an existing `Map` in simpler way
* Using `Traversal`

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

### Problems:
* In `District`, if summation of `_defaultAmount` of `[CategoryFund]` is greater than `_availableFund`, all `_defaultAmount` values should be capped by `_availableFund` in proportion to the summation of `_defaultAmount`.
* A similar requirement on `_maxAmount` of `[CategoryFund]`.
* A similar requirement on `FinalBill` if summation of `_contribute` of `[Contribution]` is greater than `_requiredFund`.

### Challenge:
* The logic of calculating the new values are the same.
* The list of data structures must be updated by the new values afterwards.  This is a typical usage of lens.

### Solution:
Defines a sharing function that calculates the new values.
```
adjustList :: Int -> Lens' a Int -> [a] -> [a]
-- Please refer to Utils.hs for the implementation
```

Build the lenses of the involved data types
```
makeLenses ''CategoryFund
makeLenses ''District
makeLenses ''Contribution
makeLenses ''FinalBill
```

Now the `FinalBill` problem can be solved by using `adjustList`
```
b & (contributeFrom %~ adjustList _requiredFund contribute)
-- b is b@FinalBill{..}
-- Please refer to FundDistribution.hs for details
```

Similarly, to solve the `District` problem
```
d & categoryFunds %~ adjustList _availableFund maxAmount
  & categoryFunds %~ adjustList _availableFund defaultAmount
-- d is d@District{..}
-- Please refer to StructureBuildup.hs for details
```

`adjustList` can also be used to recalculate an integer list.  Because `Lens' a Int` is actually `forall (f :: * -> *). Functor f => (Int -> f Int) -> a -> f a`.  That means `id` is `Lens' Int Int` such that something like `adjustList 100 id [1,2,3,4]` also works.

## Appendix
### About this application


## References
* https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
* http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
* https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses
* https://www.youtube.com/watch?v=QZy4Yml3LTY&t=882s
* https://github.com/data61/lets-lens
* A cool summary of Lens, Traversal, Iso, Prism https://www.reddit.com/r/haskell/comments/9ded97/is_learning_how_to_use_the_lens_library_worth_it/