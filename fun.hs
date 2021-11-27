join :: ([a], [a]) -> [a]
join ([], ys) = ys
join ((x : xs), ys) = x : join (xs, ys)

zipper :: ([a], [b]) -> [(a, b)]
zipper (_, []) = []
zipper ([], _) = []
zipper (x : xs, y : ys) = (x, y) : zipper (xs, ys)

zipper' :: [a] -> [b] -> [(a, b)] --zipper type
zipper' = \x y -> zipper (x, y)

partialZipper' :: [b] -> [(a, b)] --typeof zipper' []
partialZipper' = zipper' []

--zipper' [] คือนำ args ที่ pass มาจาก partialZipper' มา zipper กับ []
--simplier way คือ เก็บไว้ใน partialZipper' ตามด้านบน

fac :: (Num p, Ord p) => p -> p
fac 0 = 1
fac n =
  if n > 0
    then n * fac (n -1)
    else error "negative number"

fac' :: Integral p => p -> p
fac' n
  | n == 0 = 1
  | n > 0 = n * fac' (n -1)
  | otherwise = error "negative number"

-- fac'' :: (Num p, Ord p) => p -> p
-- fac'' n =
--   if n == 0
--     then 1
--     else
--       if n > 0
--         then n * fac'' (n -1)
--         else error "negative number"

--ยังไม่ปลอดภัย กรณีเรียก fac ที่จำนวน < 0
--เราอาจแก้ได้โดยระบุว่าถ้า input < 0 ให้บอกว่า ควรเป็น Input ที่ >= 0

fn :: Bool -> p -> p -> p
fn cond iftrue iffalse =
  if cond then iftrue else iffalse

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = join ((rev xs), [x])

--เวลาที่ join ใช้ o(n) ตาม list ตัวหน้า
--เวลาที่ rev ใช้ = o(n)
-- running time ใช้ได้แล้ว

fib :: (Num a, Num p, Ord a) => a -> p --fib n type
fib n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fib (n -1) + fib (n -2)
  | otherwise = error "negative number"

-- แบบ recursive ใช้เวลา bigO o(2^n)
-- ทำให้ดีขึ้นได้โดยเขียนแบบ iterative จะลด complexity เหลือ o(n)
