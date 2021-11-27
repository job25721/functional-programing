import Control.Applicative

ap f1 f2 l = f1 l (f2 l)

g :: (a -> a) -> [a] -> [a]
g = \f l -> ap (++) (map f) l

g' :: (a -> a) -> [a] -> [a]
g' = ap (++) . map

--pt free
--contains1 = \x l -> any (x<) l
-- = \x -> any (x<)
-- = \x -> any ((<) x)
-- = \x -> (any . <) x
-- = any . <

--contains2 = \l x -> any (x<) l
--contains2 x l = flip $ contains1 x l
--contains2 = flip $ any . <

--fmap fn with Context
b = (+ 4) <$> (Just 5)

--applicative (ap) fn in ctx with val in ctx
x = Just (+ 3) <*> (Just 1)

--Just 4
--applicative (ap) fn in with value
y = [(* 2), (+ 3)] <*> [1, 2, 3]

--[2,4,6,4,5,6]

a = (+) <$> Just 5 -- == fmap (+) (Just 5)

-- Just (+5)

-- a <*> Just 3 = Just 8

--จะเอา just 5 * just 10
--fmap just 5 กับ * ก่อน
--แล้ว ap <*> กับ just 10
fm = fmap (*) (Just 5)

--Just (*5)

res = fm <*> Just 10

--Just 50

---use liftA2
res' = liftA2 (*) (Just 5) (Just 10)

half x =
  if even x
    then Just (x `div` 2)
    else Nothing

f :: Int -> Int -> Int
f x y = x + y

t =
  let x = 1
      y = 2
   in f x y

t2 =
  let y = 2
   in let x = 1
       in f x y

z = zipWith (,) [0, 5, 3, 9] [4, 2, 0, 2, 3]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

fromMaybe (Just x) = x

p :: [a] -> Maybe a
p [] = Nothing
p (x : xs) = Just x

-- inscPair :: (Ord a) => [a] -> a
-- inscPair l =

dropFirst :: [a] -> Maybe [a]
dropFirst [] = Nothing
dropFirst (_ : xs) = Just xs

myId :: [Int]
myId = [1, 5, 0, 0, 7, 0, 1, 2, 4, 8, 9, 2, 4]

calcThaiAux :: [Int] -> [Int]
calcThaiAux [] = []
calcThaiAux (x : xs) = x * (length xs + 2) : calcThaiAux xs

-- clacThaiIdChecksum :: [Int] -> Int

calcThaiIdChecksum :: [Int] -> Maybe Int
calcThaiIdChecksum listId = liftA2 (-) (Just 11) (mod <$> sum <$> calcThaiAux <$> reverse <$> dropFirst (reverse listId) <*> Just 11)

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f x y =
  let l = zip' x y
   in map (uncurry f) l

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith'' (,)

gpa = [4.0, 4.0, 4.0]

credit :: [Integer]
credit = [3, 2, 1]

calc g c = sum (map (uncurry (\x y -> x * y)) (zip g c)) / sum c