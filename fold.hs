sum' :: (Foldable t, Num b) => t b -> b
sum' l = foldl (+) 0 l

concat' l = foldr (++) [] (reverse (l))

reverse' :: Foldable t => t a -> [a]
reverse' l = foldl (\acc cur -> cur : acc) [] l

reverse'' :: Foldable t => t a -> [a]
reverse'' l = foldr (\cur acc -> acc ++ [cur]) [] l

--

map' :: (t -> a) -> [t] -> [a]
map' f l = foldr (\cur acc -> (f cur) : acc) [] l

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p xs = foldr (\cur acc -> if p cur then cur : acc else acc) [] xs

all' :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Bool
all' pred = foldl (\acc x -> if pred x then acc else False) True

elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' y (x : xs) = if x == y then True else elem' y xs

--using fold
elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' y = foldl (\acc x -> if y == x then True else acc) False

partition' :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition' p = foldr (\x acc -> if p x then (x : fst (acc), snd (acc)) else (fst (acc), x : snd (acc))) ([], [])

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc' l = foldr (\x acc -> flip f x acc) acc' (reverse $ l)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc' l = foldl (\acc x -> flip f acc x) acc' (reverse $ l)
