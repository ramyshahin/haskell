-- ch11/QC-basics.hs
import Test.QuickCheck
import Data.List

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where   lhs = filter (< x) xs
            rhs = filter (>= x) xs
            
-------------------------------------------------         
-- QuickCheck invariants
-------------------------------------------------

-- idempotency
prop_idempotent xs = qsort (qsort xs) == qsort xs 

-- minimum 
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

-- maximum
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

-- ordered
prop_ordered xs = ordered (qsort xs)
    where   ordered []          = True
            ordered [x]         = True
            ordered (x:y:xs)    = (x <= y) && ordered (y:xs)
            
-- permutation
prop_permutation xs = permutation xs (qsort xs)
    where   permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

-- append
prop_append xs ys = 
    not (null xs) && not (null ys) ==> 
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
        
-- model-based
prop_model xs = qsort xs == sort xs
