{-# OPTIONS_GHC -O2 -Wall #-}
-------------------------------------------------------------------------------
-- Maze.hs: Functions and types for processing the maze.
--
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Proc.Maze (
      Loc
    , loc
    , getSF
    , getMoves
    , render
    , insert
    , chain) where

import Control.Lens (element)
import Control.Lens.Setter ((.~))
import Control.Monad (liftM2)
import qualified Data.Map.Lazy as M (Map,insert,lookup)
import qualified Data.ByteString.Char8 as C (ByteString,elemIndices,length
                                             ,index)
import Data.List (findIndices)

-------------------------------------------------------------------------------
-- Location type
-------------------------------------------------------------------------------
-- Loc is just a synonym for tuple that makes the program more expressive. It
-- also allows functor mapping.
newtype Loc a = Loc (a,a)
    deriving (Show,Eq,Ord)

instance Functor Loc where
    fmap f (Loc (x,y)) = Loc (f x, f y)

-- Coonstructor function
loc :: a -> a -> Loc a
loc x y = Loc (x, y)

-- EQ of fst on a tuple
getCol :: Loc a -> a
getCol (Loc (x,_)) = x
{-# INLINE getCol #-}

-------------------------------------------------------------------------------
-- Maze operations
-------------------------------------------------------------------------------
isOpen :: Char -> Bool
isOpen = (/= '#')
{-# INLINE isOpen #-}

-- Get the start and finish locations
getSF :: [C.ByteString] -> Maybe (Loc Int, Loc Int)
getSF contents
    | length (getCol s) /= 1 || length (getCol f) /= 1 = Nothing
    | otherwise = Just (fmap head s, fmap head f)
  where s = findRC 'S'
        f = findRC 'F'
        findRC ch = let rc   = map (C.elemIndices ch) contents
                        row  = findIndices (not . null) rc
                        col  = concat rc
                    in loc col row

-- Returns a location if and only if it is within the bounds of the maze
canAccess :: [C.ByteString] -> Loc Int -> Bool
canAccess xs (Loc (x, y)) = and [  
                                  x >= 0 
                                , x < C.length (head xs)
                                , y >= 0 
                                , y < length xs
                              ]
{-# INLINE canAccess #-}

-- Get all possible moves for a given location
getMoves :: [C.ByteString] -> Loc Int -> [Loc Int]
getMoves maze (Loc (x, y))
    | not . isOpen $ maze !! y `C.index` x = []
    | otherwise            = scrub nwse
  where nwse  = [loc x (y+1), loc (x+1) y, loc x (y-1), loc (x-1) y]
        scrub = filter $ canAccess maze

-- The render function produces a maze with asterisks on the shortest route 
-- using a lens
render :: [[Char]] -> [Loc Int] -> [[Char]]
render maze []               = maze
render maze (Loc (x,y):locs) = render mutate locs
    where mutate = element y . element x .~ '*' $ maze

-------------------------------------------------------------------------------
-- Operation on predecessor map
-- Predecessor map has form: children -> predecessors
-------------------------------------------------------------------------------
-- Insert a list of children into the map
insert :: [Loc Int] -> Loc Int -> M.Map (Loc Int) (Loc Int) 
                               -> M.Map (Loc Int) (Loc Int)
insert []     _ mp = mp
insert (k:ks) v mp = insert ks v (M.insert k v mp)

-- Trace the map's chain from the finish to the start
chain :: Loc Int -> Loc Int -> M.Map (Loc Int) (Loc Int) -> Maybe [Loc Int]
chain begin start = chain' begin
    where
        chain' c locs
            | c == start = Just []
            | otherwise  = 
                case M.lookup c locs of
                  Just location -> liftM2 (:) (Just c) $! chain' location locs
                  Nothing       -> Nothing
