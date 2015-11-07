{-# OPTIONS_GHC -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- Main.hs: Central bfs processing, does basic I/O, processing, and BFS
--
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Main where

import Types.Queue
import Proc.Maze
import Control.Monad (forM_)
import qualified Data.Map.Strict as M (notMember,singleton)
import qualified Data.ByteString.Char8 as C (ByteString,readFile,filter,null
                                             ,lines,putStrLn)
import System.Environment (getArgs)

bfs :: Loc -> Loc -> [C.ByteString] -> Maybe [Loc]
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
    file <- C.readFile $ head args

    let (rc, maze) = let lined = C.lines file
                         in (head lined
                             , filter (not . C.null)
                                 $ map (C.filter (/= ' '))
                                   $ tail lined)

    case getSF maze of
        Just (start,finish) ->
            case bfs start finish maze of
                Just path -> do
                    C.putStrLn rc
                    forM_ (render maze $ tail path) C.putStrLn
                Nothing   -> putStrLn "No path could be found!"
        Nothing -> putStrLn "Invalid maze."
