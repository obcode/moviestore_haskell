--------------------------------------------------------------------
-- |
-- Module    : Movies
-- Copyright : (c) Oliver Braun
-- License   : BSD3
--
-- Maintainer: Oliver Braun <ob@obraun.net>
-- Stability : provisional
-- Portability: portable
--
-- A module for dealing with movies. Uses a 'Map' for storing them.
--
-- Since this module exports a 'return' function, be sure to hide
-- 'Prelude.return', hide 'return' or import this module qualified.
--------------------------------------------------------------------

module Movies ( fromList
              , rentable
              , rent
              , insert
              , return
              , Title
              , Movies
              , showMovieStore
              ) where

import Prelude hiding (return)
import qualified Data.Map as M
import Control.Arrow ( first, second, (***) )

-- | A 'Title' is just a 'String'
type Title = String
type Available = Integer
type Rent = Integer
-- | 'Movies' is a 'Map' with 'Title' as key and the number of available
-- and rent movies as value.
type Movies = M.Map Title (Available, Rent)

-- | The 'rentable' function returns True if a movie is rentable.
rentable :: Title   -- ^ The title
         -> Movies  -- ^ All movies
         -> Bool
rentable title movies =
  case M.lookup title movies of
    Just (a,r) -> a - r > 0
    _ -> False

-- | The 'rent' function rents a movie.
-- If the movie is not rentable or even not in stock, this
-- function calls 'error'.
rent :: Title  -- ^ The title
     -> Movies -- ^ All movies
     -> Movies -- ^ The new movies
rent title movies =
  case M.lookup title movies of
    Just (a,r) ->
      if a - r > 0
      then M.adjust (second (+1)) title movies
      else error $    "rent: currently no copy of "
                   ++ title ++ " available"
    _ -> error $ "rent: " ++ title ++ " not in stock"

insert' :: Title -> Movies -> Movies
insert' title movies =
  M.insertWith (\ (a,r) -> (a+) *** (r+)) title (1,0) movies

-- | The 'insert' function inserts a new movie into the movies 'Map' and
-- returns the changed 'Map' and a flag whether the title was already
-- available or not.
insert :: Title          -- ^ The new title
       -> Movies         -- ^ All movies
       -> (Movies, Bool) -- ^ The new movies and information if the title was already in stock.
insert title movies =
  ( insert' title movies
  , M.member title movies
  )

-- | The 'return' function can be used to return a movie.
-- Returning a movie is not successful, indicated by a value of 'False' as
-- part of the result, if this title is not known or no copy of this title
-- is rent.
return :: Title         -- ^ The title
       -> Movies        -- ^ All movies
       -> (Movies,Bool) -- ^ The new movies and information if returning was successful
return title movies =
  case M.lookup title movies of
    Just (a,r) ->
      if r > 0
      then (M.insert title (a,r-1) movies, True)
      else (movies, False)
    _ -> (movies, False)

-- | The 'fromList' function can be used to generate an initial 'Map' of
-- movies from a list of titles. Duplicates in the list result in the
-- corresponding number of copies, e.g.,
--
-- > fromList ["Am Limit", "Matrix", "Am Limit", "Matrix", "Matrix", "Matrix"]
--
-- results in 'Map' containing 2 copies of *Am Limit* and 4 copies of
-- *Matrix*.
fromList :: [Title]  -- ^ A list of titles
         -> Movies   -- ^ The generated Map of movies
fromList = foldl (flip insert') M.empty

showMovie :: Title -> (Available,Rent) -> String
showMovie title (a,r) =
           title ++ " ("
        ++ (if r == 0
            then "kein "
            else "" ++ show r ++ " ")
        ++ "Exemplar" ++ (if r > 1 then "e" else "")
        ++ " verliehen,"
        ++ (if a' == 0
            then " kein "
            else " " ++ show a' ++ " ")
        ++ "Exemplar" ++ (if a' > 1 then "e" else "")
        ++ (if a' == 0
            then " mehr "
            else " ")
        ++ "auf Lager)"
   where a' = a - r

state :: Movies -> (Rent, Available)
state = M.fold (flip $ \(a,r) -> (+a) *** (+r)) (0,0)

-- | The 'showMovieStore' function returns a 'String' represantation of
-- the MovieStore.
showMovieStore :: Movies -> String
showMovieStore movies =
    M.foldrWithKey (\k v str ->  (str++) $ (++"\n") $ showMovie k v)
                  "Videotheken-Bestand\n\n" movies
     ++ "\n"
     ++ (if rent > notrent
        then "Sehr gut! Es sind mehr als die Haelfte der Videos verliehen!\n"
        else "Schlecht! Es sind zuwenig Videos verliehen!\n")
     ++ "Verliehen: " ++ show rent
     ++ ", nicht verliehen: " ++ show notrent ++ "\n\n"
    where (available,rent) = state movies
          notrent = available - rent
