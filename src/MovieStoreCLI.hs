--------------------------------------------------------------------
-- |
-- Module    : Main
-- Copyright : (c) Oliver Braun
-- License   : BSD3
--
-- Maintainer: Oliver Braun <ob@obraun.net>
-- Stability : provisional
-- Portability: portable
--
-- This module provides a CLI for a MovieStore using the "Movies" module.
--
--------------------------------------------------------------------

module Main ( main
            , readMovies
            , saveMovies
            , rentMovie
            , returnMovie
            , mainloop
            ) where

import qualified Movies as M
import System.IO(hSetBuffering, stdin, stdout, BufferMode(NoBuffering))

-- | The 'readMovies' function can be used to read a file containing
-- movies.
readMovies :: FilePath   -- ^ The name of the file
           -> IO M.Movies  -- ^ The contents as value of type 'M.Movies'
readMovies = fmap read . readFile

-- | The 'saveMovies' function can be used to write the movies to a file.
saveMovies :: FilePath  -- ^ The name of the file
           -> M.Movies    -- ^ The movies
           -> IO ()
saveMovies fp = writeFile fp . show

-- | The 'rentMovie' function can be used to rent a movie.
-- It returns a successflag and the new movies. The function asks for a
-- title and tries to rent a copy.
rentMovie :: M.Movies            -- ^ The movies
          -> IO (Bool,M.Movies)  -- ^ An indicator whether a copy was rent and the new movies.
rentMovie vl = do
  putStr "Title? "
  title <- getLine
  if M.rentable title vl
   then return (True, M.rent title vl)
   else do
    putStrLn $ "Sorry! " ++ title ++ " is currently not rentable"
    return (False,vl)

-- | The 'returnMovie' function can be used to return a movie.
-- It returns the new movies. The function asks for a
-- title and tries to put a copy it back.
returnMovie :: M.Movies     -- ^ All movies
            -> IO M.Movies  -- ^ The new movies
returnMovie vl = do
  putStr "Title? "
  title <- getLine
  let (vl',ok) = M.return title vl
   in if ok
      then return vl'
      else do
        putStrLn $ title ++ " is not ours!"
        return vl

-- | The 'main' function calls 'mainloop' with no movies.
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  mainloop $ M.fromList []

-- | The 'mainloop' function shows the following menu:
--
-- > *********************************************
-- > * Press q(uit) or one of the following keys *
-- > * g - get a movie                           *
-- > * r - return a movie                        *
-- > * l - load Movies from a file               *
-- > * s - save Movies to a file                 *
-- > * p - print state of video store            *
-- > *********************************************
-- > Input:
--
-- After a keypress the corresponding action will be performed.
--
-- Use for example:
--
-- > mainloop $ M.Movies.fromList ["Am Limit", "Matrix", "Matrix"]
mainloop :: M.Movies  -- ^ The initial movies
         -> IO ()
mainloop vl = do
    menu
    c <- getChar
    putStr "\n"
    vl' <- case c of
       'g' -> do fmap snd $ rentMovie vl
       'r' -> do returnMovie vl
       'l' -> do putStr "Filename? "
                 filename <- getLine
                 readMovies filename
       's' -> do putStr "Filename? "
                 filename <- getLine
                 saveMovies filename vl
                 return vl
       'p' -> do putStr $ M.showMovieStore vl
                 return vl
       _   -> return vl
    case c of
       'q' -> putStrLn "Bye" >> return ()
       _ -> mainloop vl'
  where
    menu :: IO ()
    menu = do
      putStrLn "*********************************************"
      putStrLn "* Press q(uit) or one of the following keys *"
      putStrLn "* g - get a movie                           *"
      putStrLn "* r - return a movie                        *"
      putStrLn "* l - load Movies from a file               *"
      putStrLn "* s - save Movies to a file                 *"
      putStrLn "* p - print state of video store            *"
      putStrLn "*********************************************"
      putStr   "Input: "

