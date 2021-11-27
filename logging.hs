import System.Random
import Text.Read

readNumber :: [Char] -> IO Int
readNumber msg = do
  putStr $ msg ++ ": "
  line <- getLine
  case readEither line :: Either String Int of
    Left e -> do
      putStrLn e
      readNumber msg
    Right n -> return n

inRange :: Ord a => (Maybe a, Maybe a) -> a -> Bool
inRange (lo, hi) guess =
  maybe True (< guess) lo
    && maybe True (> guess) hi

readGuess :: (Maybe Int, Maybe Int) -> IO Int
readGuess range = do
  guess <- readNumber "Guess"
  if inRange range guess
    then return guess
    else do
      putStrLn "Impossible answer"
      readGuess range

verdict' :: Ord a => a -> a -> (Maybe a, Maybe a) -> Either ([Char], (Maybe a, Maybe a)) [Char]
verdict' target guess (lo, hi) = do
  case compare guess target of
    EQ -> Right "You win!"
    LT -> Left ("Too low", (Just guess, hi))
    GT -> Left ("Too high", (lo, Just guess))

runGameRg :: Num t => Int -> (Maybe Int, Maybe Int) -> (t -> Bool) -> t -> [Int] -> IO ([Int], [Char])
runGameRg num range cont count list = do
  guess <- readGuess range
  let l = guess : list
  let v = verdict' num guess range
  case v of
    Right m -> do
      putStrLn m
      return (reverse l, m)
    Left (m, range') -> do
      putStrLn m
      if cont count
        then
          runGameRg
            num
            range'
            cont
            (count + 1)
            l
        else return (reverse l, "Game over")

getRange :: IO (Int, Int)
getRange = do
  lo <- readNumber "Lower bound"
  hi <- readNumber "Upper bound"
  if lo > hi
    then do
      putStrLn "Invalid range"
      getRange
    else return (lo, hi)

v4 :: IO ([Int], [Char])
v4 = do
  g <- newStdGen
  range <- getRange
  lim <- readNumber "Guess limit"
  let (num, _) = randomR range g
  runGameRg num (Nothing, Nothing) (< lim) 1 []
