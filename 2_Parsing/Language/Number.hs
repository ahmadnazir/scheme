module Language.Number where

import qualified Numeric

oct2dec x = fst $ Numeric.readOct x !! 0
hex2dec x = fst $ Numeric.readHex x !! 0
bin2dec  = bin2dec' 0
bin2dec' decint "" = decint
bin2dec' decint (x:xs) = let old = 2 * decint + (if x == '0' then 0 else 1) in
                         bin2dec' old xs
