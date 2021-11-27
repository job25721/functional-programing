newtype AndMoniod a = AndMoniod {getAndMoniod :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup (AndMoniod a) where
  (<>) = (&&)

instance Bool a => Monoid (AndMoniod a) where
  mempty = AndMoniod True
  (AndMoniod x) <> (AndMoniod y) = AndMoniod (x <> y)

newtype OrMonoid a = OrMonoid {getOrMonoid :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup (OrMonoid a) where
  (<>) = (||)

instance Bool a => Monoid (OrMonoid a) where
  mempty = OrMonoid False
  (OrMonoid x) <> (OrMonoid y) = OrMonoid (x <> y)

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

listBind :: [a] -> (a -> [b]) -> [b]
listBind [] _ = []
listBind (x : xs) f = f x <> listBind xs f

eitherBind :: Either r a -> (a -> Either r b) -> Either r b
eitherBind (Left a) _ = Left a
eitherBind (Right a) f = f a

arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
arrowBind f g = \r -> g (f r) $ r

pairBind :: (r, a) -> (a -> (r, b)) -> (r, b)
pairBind (r, a) f = f $ snd (r, a)
-- no need to know typeof r
