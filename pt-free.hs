filterConcat = (concat .) . filter

mapFilter = \oper pred l -> map oper (filter pred l)

x :: (a -> b) -> (a -> Bool) -> [a] -> [b]
x = (. filter) . (.) . (map)

ap f1 f2 l = f1 l (f2 l)

g :: (a -> a) -> [a] -> [a]
g = (.) (ap (++)) map

-- any :: (a -> Bool) -> [a] -> Bool

-- contains1 = \x l -> any (x <) l

lenComp :: [a] -> Int
lenComp = \l -> sum [length [x] | x <- l]

ex = [(x, y) | x <- [2, 3, 5], y <- [1, 2, 4], even $ x + y]

crossProdAux :: t -> [b] -> [(t, b)]
crossProdAux _ [] = []
crossProdAux x (a : b) = (x, a) : (crossProdAux x b)

evenPair :: Integral a => [a] -> [a] -> [(a, a)]
evenPair [] ys = []
evenPair (a : b) ys = filter (\(x, y) -> even $ (x + y)) ((crossProdAux a ys) ++ (evenPair b ys))

contains1' :: (Foldable t, Ord a) => a -> t a -> Bool
contains1' = any . (<)

contains2' :: (Foldable t, Ord a) => t a -> a -> Bool
contains2' = flip (any . (<))

foo [] = "empty list"
foo [x] = "sigleton list"
foo (x1 : (x2 : xs)) = "short list"
foo (x1 : (x2 : (x3 : xs))) = "long list"
