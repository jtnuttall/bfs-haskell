{-# OPTIONS_GHC -O2 -Wall #-}
-------------------------------------------------------------------------------
-- Maze.hs: Functions and types for processing the maze.
--
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Proc.Maze (
      Loc
    , loc
    , parseMaze
    , getSF
    , getMoves
    , render
    , insert
    , chain) where

import Control.Lens (element)
import Control.Lens.Setter ((.~))
import Control.Monad (forM_,liftM2)
import Control.Monad.ST (runST)
import qualified Data.Map.Strict as M (Map,insert,lookup)
import Data.STRef (readSTRef, modifySTRef, newSTRef)
import Data.List (findIndices)
import Data.Maybe (fromMaybe)
import Prelude hiding (init)

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

-------------------------------------------------------------------------------
-- Maze operations
-------------------------------------------------------------------------------
-- Parse maze as list of bools (true for open, false for wall)
parseMaze :: [[Char]] -> [[Bool]]
parseMaze = fmap $ fmap (/= '#')

-- Get the start and finish locations
getSF :: [[Char]] -> Maybe (Loc Int, Loc Int)
getSF contents
    | length (getCol s) /= 1 || length (getCol f) /= 1 = Nothing
    | otherwise = Just (fmap head s, fmap head f)
  where s = findRC 'S'
        f = findRC 'F'
        findRC ch = let init = map (findIndices (== ch)) contents
                        row  = findIndices (not . null) init
                        col  = concat init
                    in loc col row

-- Returns a location if and only if it is within the bounds of the maze
accMaybe :: [[a]] -> Loc Int -> Maybe a
accMaybe xs (Loc (x, y))
    | inBounds  = Just $ xs !! y !! x
    | otherwise = Nothing
  where
      inBounds = and [  
                        x >= 0 
                      , x < length (head xs)
                      , y >= 0 
                      , y < length xs
                     ]

-- Get all possible moves for a given location
getMoves :: [[Bool]] -> Loc Int -> [Loc Int]
getMoves maze (Loc (x, y))
    | not $ maze !! y !! x = []
    | otherwise            = scrub nwse
  where nwse  = [loc x (y+1), loc (x+1) y, loc x (y-1), loc (x-1) y]
        scrub = filter $ fromMaybe False . accMaybe maze

-- The render function produces a maze with asterisks on the shortest route 
-- using a lens
render :: [[Char]] -> [Loc Int] -> [[Char]]
render maze []               = maze
render maze (Loc (x,y):locs) = render lensed locs
    where lensed = element y . element x .~ '*' $ maze

-------------------------------------------------------------------------------
-- Operation on predecessor map
-- Predecessor map has form: children -> predecessors
-------------------------------------------------------------------------------
-- Insert a list of children into the map
insert :: [Loc Int] -> Loc Int -> M.Map (Loc Int) (Loc Int) 
                               -> M.Map (Loc Int) (Loc Int)
insert ks v mp = runST $ do
    m <- newSTRef mp
    forM_ ks $ \k -> modifySTRef m $ M.insert k v
    readSTRef m

-- Trace the map's chain from the finish to the start
chain :: Loc Int -> Loc Int -> M.Map (Loc Int) (Loc Int) -> Maybe [Loc Int]
chain begin start = chain' begin
    where
        chain' c locs
            | c == start = Just []
            | otherwise  = 
                case M.lookup c locs of
                  Just location -> liftM2 (:) (Just c) $ chain' location locs
                  Nothing       -> Nothing
