data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node l x r) =
  [x] ++ preorder l ++ preorder r

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node l x r) =
  inorder l ++ [x] ++ inorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node l x r) =
  postorder l ++ postorder r ++ [x]

mapTree :: (t -> a) -> Tree t -> Tree a
mapTree _ Empty = Empty
mapTree p (Node l x r) = Node (mapTree p l) (p x) (mapTree p r)

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree _ acc Empty = acc
foldlTree f acc t = foldl f acc (preorder t)

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ acc Empty = acc
foldrTree f acc t = foldr f acc (preorder t)

height :: (Num p, Ord p) => Tree a -> p
height Empty = 0
height (Node l _ r) = 1 + max (height l) (height r)

getChildVal :: Tree a -> a
getChildVal (Node _ v _) = v

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node l x r) = case (l, x, r) of
  (Empty, _, Empty) -> True
  (_, _, Empty)
    | getChildVal l < x -> isBST l
    | otherwise -> False
  (Empty, _, _)
    | getChildVal r > x -> isBST r
    | otherwise -> False
  (_, _, _)
    | (getChildVal l < x) && getChildVal r > x -> isBST l && isBST l
    | otherwise -> False

tree :: Tree Integer
tree = Node (Node (Node Empty 5 Empty) 4 (Node Empty 1 Empty)) 9 (Node (Node Empty 6 Empty) 7 (Node Empty 0 Empty))

tree2 :: Tree Integer
tree2 = Node (Node (Node (Node Empty 7 Empty) 8 Empty) 9 Empty) 10 (Node (Node Empty 5 Empty) 12 (Node Empty 17 Empty))

tree3 :: Tree Integer
tree3 = Node (Node Empty 4 Empty) 9 (Node Empty 11 Empty)
