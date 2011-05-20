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

type Title = String
type Available = Integer
type Rent = Integer
type Movies = M.Map Title (Available, Rent)

rentable :: Title -> Movies -> Bool
rentable title movies =
  case M.lookup title movies of
    Just (a,r) -> a - r > 0
    _ -> False

rent :: Title -> Movies -> Movies
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

insert :: Title -> Movies -> (Movies, Bool)
insert title movies =
  ( insert' title movies
  , M.member title movies
  )

return :: Title -> Movies -> (Movies,Bool)
return title movies =
  case M.lookup title movies of
    Just (a,r) ->
      if r > 0
      then (M.insert title (a,r-1) movies, True)
      else (movies, False)
    _ -> (movies, False)

fromList :: [Title] -> Movies
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
