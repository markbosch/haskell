act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

readInt :: IO Int
readInt = do
  x <- getLine
  return (read x :: Int)

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do
  x  <- readInt
  xs <- readInts (n - 1)
  return (x:xs)

adder :: IO ()
adder = do putStr "How many numbers? "
           n  <- readInt
           ns <- readInts n
           putStr "The total is "
           putStrLn $ show $ sum ns
           return ()

