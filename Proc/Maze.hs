{-# OPTIONS_GHC -O2 -Wall #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-------------------------------------------------------------------------------
-- Maze.hs: Functions and types for processing the maze.
--
-- Author: Jeremy Nuttall
-------------------------------------------------------------------------------
module Proc.Maze (
      Loc
    , getSF
    , getMoves
    , render
    , insert
    , chain) where

import GHC.Generics (Generic)
import Control.Lens (ix)
import Control.Lens.Setter ((.~))
import Control.Monad (liftM2)
import Control.DeepSeq (NFData,($!!))
import qualified Data.Map.Lazy as M (Map,insert,lookup)
import qualified Data.ByteString.Char8 as C (ByteString,elemIndices,length
                                             ,index)
import Data.List (findIndices)

-------------------------------------------------------------------------------
-- Location type
-------------------------------------------------------------------------------
data Loc = Loc !Int !Int
    deriving (Generic,Eq,Ord,NFData)

-- Coonstructor 
loc :: Int -> Int -> Loc
loc = Loc
{-# INLINE loc #-}

-------------------------------------------------------------------------------
-- Maze operations
-------------------------------------------------------------------------------
isOpen :: Char -> Bool
isOpen = (/= '#')
{-# INLINE isOpen #-}

lNE1 :: [a] -> Bool
lNE1 (_:_:_) = True
lNE1 []      = True
lNE1 _       = False
{-# INLINE lNE1 #-}

-- Get the start and finish locations
getSF :: [C.ByteString] -> Maybe (Loc, Loc)
getSF contents = 
  case (start,finish) of
      (Just s, Just f)   -> Just (s,f)
      (Nothing, Just _)  -> Nothing
      (Just _, Nothing)  -> Nothing
      (Nothing, Nothing) -> Nothing
  where start     = findRC 'S'
        finish    = findRC 'F'
        findRC ch = let rc   = map (C.elemIndices ch) contents
                        row  = findIndices (not . null) rc
                        col  = concat rc
                    in if lNE1 col || lNE1 row then Nothing
                       else Just $ loc (head col) (head row)
{-# INLINE getSF #-}

-- Returns a location if and only if it is within the bounds of the maze
canAccess :: [C.ByteString] -> Loc -> Bool
canAccess xs (Loc x y) =    x >= 0 
                         && x < C.length (head xs) 
                         && y >= 0 
                         && y < length xs
{-# INLINE canAccess #-}

-- Get all possible moves for a given location
getMoves :: [C.ByteString] -> Loc -> [Loc]
getMoves maze (Loc x y)
    | not . isOpen $ maze !! y `C.index` x = []
    | otherwise                            = scrub nwse
  where nwse  = [loc x (y+1), loc (x+1) y, loc x (y-1), loc (x-1) y]
        scrub = filter (canAccess maze)
{-# INLINE getMoves #-}

-- The render function produces a maze with asterisks on the shortest route 
-- using a lens (asterisks have ASCII value 42)
render :: [C.ByteString] -> [Loc] -> [C.ByteString]
render maze []               = maze
render maze ( Loc x y :locs) = render (ix y . ix x .~ 42 $ maze) locs
{-# INLINABLE render #-}

-------------------------------------------------------------------------------
-- Operation on predecessor map
-- Predecessor map has form: children -> predecessors
-------------------------------------------------------------------------------
-- Insert a list of children into the map
insert :: [Loc] -> Loc -> M.Map Loc Loc -> M.Map Loc Loc
insert []     _ mp = mp
insert (k:ks) v mp = insert ks v (M.insert k v mp)

-- Trace the map's chain from the finish to the start
chain :: Loc -> Loc -> M.Map Loc Loc -> Maybe [Loc]
chain begin start = chain' begin
    where
        chain' c locs
            | c == start = Just []
            | otherwise  = 
                case M.lookup c locs of
                  Just location -> liftM2 (:) (Just c) $!! chain' location locs
                  Nothing       -> Nothing
