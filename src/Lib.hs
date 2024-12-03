module Lib where 

import qualified Data.Text as T
import qualified Data.Text.Read as T

-- Library of common functions across multiple days

-- convert Test to Int, assuming input will be positive integers
textToInt :: T.Text -> Int
textToInt str = 
    case (T.decimal str) of
        Right (n,_) -> n
        Left _ -> 0