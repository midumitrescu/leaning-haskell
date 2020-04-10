module Cis194.Hw1.TowersOfHanoi where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a dest c = [(a, dest)]
hanoi n source destination buffer =
  hanoi (n - 1) source buffer destination ++ [(source, destination)] ++ hanoi (n - 1) buffer destination source
