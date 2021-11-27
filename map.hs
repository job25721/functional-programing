filterConcat :: ([a] -> Bool) -> [[a]] -> [a]
filterConcat _ [] = []
filterConcat fn (x : xs)
  | fn x = x ++ filterConcat fn xs
  | otherwise = filterConcat fn xs

filterConcat' :: ([a] -> Bool) -> [[a]] -> [a]
filterConcat' _ [] = []
filterConcat' pred l = concat (filter pred l)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn list =
  reverse (aux list [])
  where
    aux [] res = res
    aux (x : xs) res
      | fn x = aux xs (x : res)
      | otherwise = res

x = filterConcat ((< 3) . length) [[1, 2, 3], [4], [5, 6], [], [7, 8, 9, 10]]

ap f1 f2 l = f1 l (f2 l)

g :: (a -> a) -> [a] -> [a]
g = \f l -> l ++ map f l

g' = (ap (++)) . (map)
