import Data.Void

fn1 :: (a -> b -> c) -> (a -> b) -> a -> c
fn1 g f a = g a (f a)

--จาก (a -> b -> c) -> (a -> b) -> a สามารถ proof c ได้เลยจากทั้ง
--สาม args โดยจาก args แรก คือ function ที่รับ a และ
--b และได้ c ก็คือใช้ a ปลดล๊อคได้ b และใช้ b ปลดล๊อคได้ c โดยจาก args ที่สามจะได้ a จึง
--เหลือแค่ b ที่เราต้องใช้ไขเพื่อให้ได้ c โดย b  หาได้จาก args ที่ 2 ที่สองซึ่งคือ function ที่
--ต้องใช้ a ปลดล๊อค จะได้ b ออกมา ดังนั้น สรุปได้ว่าเราสามารถ proof c ได้

fn2 :: (a, b) -> Either a b
fn2 (_, b) = Right b
fn2 (a, _) = Left a

-- proof by cases
--มี function ที่รับ a และ b ให้เลือกว่าจะคืนค่า a หรือ b กลับไป
--ผ่าน Left หรือ Right ซึ่งก็คือ Either a b

fn3 :: (a -> b, a) -> b
fn3 (f, a) = f a

--สามารถ proof b ได้จาก args แรก คือ pair function a->b และ a
--คือหา b ได้จากการ apply a ลงใน a->b เพื่อปลดล๊อค จะได้ b

fn4 :: Either (a -> Void) b -> a -> b
fn4 (Right b) _ = b

--เนื่องจาก a -> Void ซึ่งเป็น Left ของ either คือ False
--ดังนั้น เราจึงไม่สนใจ Left จึงรับ Right ซึ่งก็คือ b และจะเห็นว่า
--ต้องรับ a มา แต่ผลลัพธ์สุดท้ายคือ b ดังนั้นจึงใช้จาก Right เลยซึ่งก็คือ b
