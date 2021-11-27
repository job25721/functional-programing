-- rev(l) :
--     result = []
--     while l is not empty:
--         x = l.remove(0)
--         result = x : result
--     return result

-- rev' (l) : return rev_aux (l, [])

-- rev_aux (l,result):
--     if l is empty : return result
--     else :
--         x = l.remove(0)
--         return rev_aux(l,x:result)

--haskell tail-recursion lang

rev' l =
  aux l []
  where
    aux [] res = res
    aux (x : xs) res = aux xs (x : res)

sum' l =
  aux l 0
  where
    aux [] res = res
    aux (x : xs) res = aux xs (x + res)

fac' n
  | n >= 0 = aux n 1
  | otherwise = error "negative number"
  where
    aux 0 res = res
    aux n res = aux (n -1) (res * n)

fib' n
  | n >= 0 = aux n 0 1
  | otherwise = error "negative number"
  where
    aux 0 x _ = x
    aux 1 _ y = y
    aux n x y = aux (n -1) y (x + y) --all aux is scope variable

len [] = 0
len (x : xs) = 1 + len xs

-- list_map :: (t -> a) -> [t] -> [a]
list_map :: (t -> a) -> [t] -> [a]
list_map fn l =
  aux l []
  where
    aux [] res = reverse res
    aux (x : xs) res = aux xs (fn x : res)

test1 = print (list_map reverse ["hello", "world"]) --output -> ["olleh","dlrow"]

test2 = print (list_map fib' [10, 20, 30, 40]) --output -> [55,6765,832040,102334155]

test3 = print (list_map fac' [5, 10]) --output -> [120,3628800]

zipper :: [a] -> [b] -> [(a, b)]
zipper x y =
  aux x y []
  where
    aux [] _ res = reverse res
    aux _ [] res = reverse res
    aux (x : xs) (y : ys) res = aux xs ys ((x, y) : res)
