module Quaint where

import Prelude hiding (exponent)

data Exp = Exp {basis :: !String, exponent :: !Integer} deriving (Show, Eq)

instance Num Exp where
  negate e = e {exponent = negate (exponent e)}
  (+) e1 e2 = e1 {exponent = exponent e1 + exponent e2}
  (*) e1 e2 = e1 {exponent = exponent e1 * exponent e2} -- unused
  fromInteger e = Exp {basis = "", exponent = e} -- unused
  abs e = e {exponent = abs (exponent e)} -- unused
  signum e = e {exponent = signum (exponent e)} -- unused

add :: [Exp] -> [Exp] -> [Exp]
add (hd1 : tl1) (hd2 : tl2)
  | basis hd1 == basis hd2 = (hd1 + hd2) : add tl1 tl2
  | otherwise = hd1 : add tl1 (hd2 : tl2)
add [] (hd : tl) = hd : tl -- exclusive cases
add e [] = e

diff :: [Exp] -> [Exp] -> [Exp]
diff (hd1 : tl1) (hd2 : tl2)
  | basis hd1 == basis hd2 = (hd1 - hd2) : diff tl1 tl2
  | otherwise = hd1 : diff tl1 (hd2 : tl2)
diff [] (hd : tl) = -hd : diff [] tl
diff e [] = e
