data COp a = CVal Int a
  deriving (Show)

instance Functor COp where
  fmap f (CVal c v) = CVal (c + 1) (f v)

-- class MyFunctor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance MyFunctor ((->) r) where
--   fmap f g = f . g

maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp _ Nothing = Nothing
maybeAp Nothing _ = Nothing
maybeAp (Just f) (Just a2) = Just (f a2)

initMaybe :: a -> Maybe a
initMaybe = Just

initList :: a -> [a]
initList a = [a]

initPair :: a -> b -> (a, b)
initPair a b = (a, b)

listAp :: [a -> b] -> [a] -> [b]
listAp [] _ = []
listAp _ [] = []
listAp l1 l2 = [f y | f <- l1, y <- l2]

x :: Num a => b -> (a, b)
x = (,) 1