msg :: [Char] -> [Char]
msg n = "Player 2 - Please enter your number. (Round = " ++ n ++ " ) : "

main :: IO ()
main = do
  putStr "Player 1 - Please enter your number : "
  start <- getLine
  putStr "Player 1 - Please enter your number to limit round : "
  n <- getLine
  if (read n) <= 0
    then putStrLn "Sorry, please enter positive integer."
    else main' start n
  return ()

main' :: String -> String -> IO ()
main' start n = do
  let nn = (read n) - 1
  putStr (msg n)
  random <- getLine
  if start == random
    then do
      putStr "Correct! number is "
      putStrLn start
      putStrLn "Player 2 Win!!!"
      return ()
    else
      if nn == 0
        then putStrLn "Lose!!! Player 1 Win!!!"
        else main' start (show nn)
