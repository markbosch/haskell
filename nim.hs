-- Nim
-- Two player game. The one who has the board empty wins
--
-- 1: *****
-- 2: ****
-- 3: ***
-- 4: **
-- 5: *

import Data.Char

-- Players one and two
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1] -- number of stars on the initial board per row

finished :: Board -> Bool
finished = all (== 0)


-- A move in the game is speficied by a row number an the number of stars to
-- be removed, and is valid if the row contains at least this many stars.

-- valid
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

-- move
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
   where update r n = if r == row then n-num else n

-- IO

putRow :: Int -> Int -> IO ()
putRow row num = do
   putStr (show row)
   putStr ": "
   putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do
   putRow 1 a
   putRow 2 b
   putRow 3 c
   putRow 4 d
   putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
   putStr prompt
   x <- getChar
   newline
   if isDigit x then
      return (digitToInt x)
   else
      do putStrLn "ERROR: Invalid digit"
         getDigit prompt

newline :: IO ()
newline = putChar '\n'

-- Game main loop
play :: Board -> Int -> IO ()
play board player =
   do newline
      putBoard board
      if finished board then
         do newline
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins!!"
      else
         do newline
            putStr "Player "
            putStrLn (show player)
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove: "
            if valid board row num then
               play (move board row num) (next player)
            else
               do newline
                  putStrLn "ERROR: Invalid move"
                  play board player

nim :: IO ()
nim = play initial 1