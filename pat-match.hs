partition :: (a -> Bool) -> [a] -> ([a], [a])
--partition ทำหน้าที่ filter list ที่รับเข้าไป
--และ return ออกมาเป็น pair ของ list
--ซึ่งตัวหน้า คือสิ่งที่ filter ได้ ตัวหลังคือตัวที่ไม่เข้าเงื่อนไข

partition _ [] = ([], [])
partition p (x : xs)
  | p x = (x : l, r)
  | otherwise = (l, x : r)
  where
    (l, r) = partition p xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p list = fst (partition p list)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot : xs) =
  quicksort (fst (partition (< pivot) xs))
    ++ [pivot]
    ++ quicksort (fst (partition (>= pivot) xs))

-- Ordering เป็น Data type ใน class Eq ที่ return มาจาก compare มี 3 แบบคือ LT EQ และ GT
-- มี 3 constructor
-- มี 3 วิธีที่จะ match pattern