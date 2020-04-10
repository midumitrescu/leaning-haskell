module Mit.Hw1 where

diff :: (Float -> Float) -> Float -> Float -> Float
diff f dx x = (f (x + dx) - f x) / dx


diffOverall f dx = \x -> diff f dx x


newton_iter :: (Float -> Float) -> Float -> Float -> Integer -> Float
newton_iter _ x _ 0 = x
newton_iter f x dx k =
            val - f (val) / (diff f dx val)
            where val = newton_iter f x dx (k - 1)


type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f = f

emptySet :: IntSet
emptySet _ = False

allInts :: IntSet
allInts _ = True

interval :: Int -> Int -> IntSet
interval lowerBound upperBound = \x -> lowerBound <= x && x <= upperBound

greatestFactor :: Int -> Int -> Int
greatestFactor x 0 = x
greatestFactor x y | y > x = greatestFactor y x

greatestFactor x y = greatestFactor y (x `mod` y)

isRelativePrime :: Int -> Int -> Bool
x `isRelativePrime` y = greatestFactor x y == 1

relativePrimesOf :: Int -> IntSet
relativePrimesOf x y = x `isRelativePrime` y

-- Boolean Operators
setIntersection :: IntSet -> IntSet -> IntSet
setIntersection a b x = a x && b x

setUnion :: IntSet -> IntSet -> IntSet

setUnion a b x = a x || b x

setComplement :: IntSet -> IntSet
setComplement a x = not (a x)

-- Set generation
addToSet :: Int -> IntSet -> IntSet
addToSet x = setUnion (\y -> x == y)

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet x a =  emptySet

