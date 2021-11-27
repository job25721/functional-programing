class IfValue a where
  boolVal :: a -> Bool

instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Double where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Float where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Bool where
  boolVal False = False
  boolVal True = True

instance IfValue (Maybe a) where
  boolVal Nothing = False
  boolVal _ = True

instance IfValue [a] where
  boolVal [] = False
  boolVal _ = True

mapMaybe :: (t -> a) -> Maybe t -> Maybe a
mapMaybe f x = case x of
  Just a -> Just (f a)
  Nothing -> Nothing

mapPair :: (t -> b) -> (t, t) -> (b, b)
mapPair f (a, b) = (,) (f a) (f b)
