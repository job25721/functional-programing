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

verdict :: Ord a => a -> a -> Either [Char] [Char]
verdict target guess = do
  case compare guess target of
    EQ -> Right "You win!"
    LT -> Left "Too low"
    GT -> Left "Too high"

runGameRg num range cont count = do
  guess <- readGuess range
  let v = verdict' num guess range
  case v of
    Right m -> do
      putStrLn m
    Left (m, range') -> do
      putStrLn m
      if cont count
        then
          runGameRg
            num
            range'
            cont
            (count + 1)
        else putStrLn "Game over"

runGame :: Num t => Int -> (t -> Bool) -> t -> IO ()
runGame num cont count = do
  guess <- readNumber "Guess"
  let v = verdict num guess
  case v of
    Right m -> do
      putStrLn m
    Left m -> do
      putStrLn m
      if cont count
        then runGame num cont (count + 1)
        else putStrLn "Game over"

v3' :: IO ()
v3' = do
  num <- randomIO :: IO Int
  lim <- readNumber "Guess limit"
  runGame num (< lim) 1

getRange = do
  lo <- readNumber "Lower bound"
  hi <- readNumber "Upper bound"
  if lo > hi
    then do
      putStrLn "Invalid range"
      getRange
    else return (lo, hi)

v4 = do
  g <- newStdGen
  range <- getRange
  lim <- readNumber "Guess limit"
  let (num, _) = randomR range g
  runGame num (Nothing, Nothing) (< lim) 1

nRandomRs :: (RandomGen g, Random a, Integral n) => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs (a, b) n gen =
  let (val, gen') = randomR (a, b) gen
      (rest, gen'') = nRandomRs (a, b) (n -1) gen'
   in (val : rest, gen'')
