module Lib
    (someLibFunc, minima, MaybeMon(JustMon, NothingMon), _lift, (Lib.>>=), plus) where

import Data.List

--data JustMon a
--data NothingMon
data MaybeMon a = JustMon a | NothingMon deriving (Show, Eq)

_lift :: a -> MaybeMon a
_lift x = JustMon x

(>>=) :: MaybeMon a -> (a -> MaybeMon b) -> MaybeMon b
NothingMon  >>= _ = NothingMon
JustMon x >>= f = f x

plus :: Int -> MaybeMon Int -> MaybeMon Int
x `plus` my = my Lib.>>= (\y -> _lift (y + x))

(-) :: Int -> MaybeMon Int -> MaybeMon Int
_ - NothingMon = NothingMon
x - JustMon y = _lift (x Prelude.- y)


someLibFunc :: IO ()
someLibFunc = putStrLn "MIIII 3"

minima k xs = take k (sort xs)


