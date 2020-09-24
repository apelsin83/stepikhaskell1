module Comp where

import Data.Char (toUpper) -- only 1
import Data.Char hiding (toLower) -- exclude 1

-- import Data.List (union)
-- import Data.Set (union)


-- import Data.List 
-- import qualified Data.Set -- means all f should use prefix

-- or 
-- mport qualified Data.Set as Set

-- import Prelude hiding ..

-- EXPORT

import Test

-- f1 = const42 True

f2 = sumIt 3 4