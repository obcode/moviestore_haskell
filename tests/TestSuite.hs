module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Control.Arrow (second)
import Data.List
import qualified Data.Map as M

import Movies

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "QuickCheck Tests" [
                testProperty "All titles in movies using fromList." prop_fromList1,
                testProperty "All titles in movies using fromList with correct number of copies." prop_fromList2,
                testProperty "All titles are rentable." prop_fromListRentable
            ],
        testGroup "HUnit Tests - One copy" [
                testCase "All titles are rentable." test_rentable,
                testCase "After renting the sole copy, the title is not rentable anymore." test_rentAndThenNotRentable,
                testCase "After renting one copy and returning it, we have the same map of movies." test_rentAndReturnEqual
            ],
        testGroup "HUnit Tests - Two copies" [
                testCase "After renting one of two copies, the title is still rentable." test_rentAndStillAnotherRentable,
                testCase "After renting the sole copy and returning it, the title is rentable again." test_rentReturnAndThenRentable
            ]
    ]

-- QuickCheck tests

prop_fromList1 xs = (not (null xs)) ==>
        and $ map (flip elem $ M.keys $ fromList xs) $ nub xs
  where types = (xs :: [String])

prop_fromList2 xs = (not (null xs)) ==>
        and $ map (flip elem $ onlyAvailable $ M.toList $ fromList ys) $ titleCount ys
  where ys = xs ++ xs
        types = (xs :: [String])
        onlyAvailable =  map (second fst)
        titleCount = foldl (flip insert') []
        insert' t xs =
          let (found, notElem) = partition ((==t) . fst) xs
          in ( case found of
                [] -> (t,1)
                [(_,c)] -> (t, c+1)
                _ -> error "duplicate entries"
             : notElem )

-- HUnit tests

prop_fromListRentable xs = (not (null xs)) ==>
  and $ map (flip rentable $ fromList xs) xs

test_rentable =
  let movieList = ["Am Limit", "Matrix", "Fire & Ice"]
      movies = fromList movieList
  in assertBool ("Movies not rentable")
                $ and $ map (flip rentable movies) movieList

test_rentAndThenNotRentable =
  let movie = "Am Limit"
      movies = fromList [movie]
  in assertBool ("Movie \""++movie++"\" still rentable")
                $ not $ rentable movie $ rent movie movies

test_rentAndStillAnotherRentable =
  let movie = "Am Limit"
      movies = fromList [movie, movie]
  in assertBool ("Movie \""++movie++"\" not a second time rentable")
                $ rentable movie $ rent movie movies

test_rentReturnAndThenRentable =
  let movie = "Am Limit"
      movies = fromList [movie]
  in assertBool ("Movie \""++movie++"\" not rentable")
                $ rentable movie $ fst $ Movies.return movie $ rent movie movies

test_rentAndReturnEqual =
  let movie = "Am Limit"
      movies = fromList [movie]
  in assertEqual ("Movies not equal after rent and return")
                 (fst $ Movies.return movie $ rent movie movies) movies

