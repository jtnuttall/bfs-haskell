{-# OPTIONS_GHC -O2 -Wall #-}
-------------------------------------------------------------------------------
-- Main.hs: Central bfs processing, does basic I/O, processing, and BFS
--
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Main where

import Types.Queue
import Proc.Maze
import Control.Monad (forM_)
import qualified Data.Map.Lazy as M (notMember,singleton)
import System.Environment (getArgs)

bfs :: Loc Int -> Loc Int -> [[Bool]] -> Maybe [Loc Int]
bfs start finish maze = 
    let preds = M.singleton start finish
        q     = singleton start
    in chain finish start $ search preds q
    
    where
        search preds q
            | empty q          = preds
            | not $ null moves = search (insert moves l preds) q''
            | otherwise        = search preds q''
          where
              (l, q') = pop q
              moves   = filter (`M.notMember` preds) $ getMoves maze l
              q''     = listApp moves q'

main :: IO()
main = do
    args <- getArgs
    file <- readFile $ head args

    let (rc, maze) = let lined = lines file 
                          in (head lined
                              , filter (not . null) 
                                  $ map (filter (/= ' ')) 
                                     $ tail lined)

    case getSF maze of
        Just (s,f) -> 
            case bfs s f $ parseMaze maze of
                Just path -> do
                    putStrLn rc
                    forM_ (render maze $ tail path) putStrLn
                Nothing   -> putStrLn "No path could be found!"
        Nothing   -> putStrLn "Invalid maze."
