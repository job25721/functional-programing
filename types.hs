data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show)

daysInMonth :: Num p => Month -> p
daysInMonth m = case m of
  January -> 31
  February -> 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

nextMonth :: Month -> Month
nextMonth m = case m of
  January -> February
  February -> March
  March -> April
  April -> May
  May -> June
  June -> July
  July -> August
  August -> September
  September -> October
  October -> November
  November -> December
  December -> January

nextDay :: (Ord a, Num a) => a -> Month -> (a, Month)
nextDay d m
  | d < daysInMonth m = (d + 1, m)
  | otherwise = (1, nextMonth m)
