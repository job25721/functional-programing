s :: (a -> b -> c) -> (a -> b) -> a -> c
s a b c = a c (b c)

k :: a -> b -> a
k a _ = a

i :: a -> a
i a = a

g :: a -> (a -> b) -> b
g a f = s (flip k) (i f) (i a)

h :: (a -> b -> c) -> b -> a -> c
h f b a = s f (k b) (i a)