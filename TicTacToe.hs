-- Tic-Tac-Toe

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

-- B is a blank space
data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities
empty :: Grid
empty = replicate size (replicate size B)

-- concat the grid, which will flatten it and then check if all is not B
full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X         -- if Os are less or eq then Xs then O turn else X
         where
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)
           ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
             line = all (== p)
             rows = g
             cols = transpose g
             dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Display a grid

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

-- e.g
-- putGrid [[B,O,O],[O,X,O],[X,X,X]]

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar    = replicate 3 "|"

-- e.g
-- showRow [O,B,X]
-- ["   |    |   ",
--  " O |    | X ",
--  "   |    |   "]

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

-- if valid, split the grid -> replace the B with the player
-- and reform the grid
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

-- break a list into maximal segments of a given length
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a number

getNat :: String -> IO Int
getNat prompt =
  do putStr prompt
     xs <- getLine
     if xs /= [] && all isDigit xs then
       return (read xs)
     else
       do putStrLn "ERROR: Invalid number"
          getNat prompt

-- Human vs Human

-- Main
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p =
  do cls
     goto (1,1)
     putGrid g
     run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  []   -> do putStrLn "ERROR: Invalid move"
                             run' g p
                  [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter move: "

-- clear the screen
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Computer
-- Game Tree

data Tree a = Node a [Tree a]
              deriving Show

-- build a game tree
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

-- > gametree empty O --> :')

moves :: Grid -> Player -> [Grid]
moves g p
  | won g     = []
  | full g    = []
  | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

-- Prune will prevent that a gametree can become very large and will cut it off
-- at a certain depth
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts] 

-- > prune 5 (gametree empty O)

depth :: Int
depth = 9

-- minimax algorithm
-- This algorithm will label every node in the tree with
--  - Leaves -> winning player or blank otherwise
--  - Nodes, will be labelled with minimum or maximum of
--    the player labels from the child nodes one level down
--
--                 O
--               / | \
--              /  |  \
--             /   |   \
--            X    O    X  
--           / \  / \  / \
--          X   O O  O O  X
--              | |  | |
--              O O  O O

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
  | wins O g  = Node (g,O) []
  | wins X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
                  where
                    ts' = map minimax ts
                    ps  = [p | Node (_,p) _ <- ts']
            
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                 tree = prune depth (gametree g p)
                 Node (_,best) ts = minimax tree

-- Human vs Computer

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "It's a draw!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p
                    [g'] -> play g' (next p)
  | p == X   = do putStr "Player X is thinking... "
                  (play $! (bestmove g p)) (next p)

-- ghc -O2 TicTacToe.hs
-- ./TicTacToe
