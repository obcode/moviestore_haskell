module Main where

import qualified Movies as M
import System.IO(hSetBuffering, stdin, stdout, BufferMode(NoBuffering))

type Movies = M.Movies

readMovies :: FilePath -> IO Movies
readMovies = fmap read . readFile

saveMovies :: FilePath -> Movies -> IO ()
saveMovies fp = writeFile fp . show

rentVideo :: Movies -> IO (Bool,Movies)
rentVideo vl = do
  putStr "Title? "
  title <- getLine
  if M.rentable title vl
   then return (True, M.rent title vl)
   else do
    putStrLn $ "Sorry! " ++ title ++ " is currently not rentable"
    return (False,vl)

returnVideo :: Movies -> IO Movies
returnVideo vl = do
  putStr "Title? "
  title <- getLine
  let (vl',ok) = M.return title vl
   in if ok
      then return vl'
      else do
        putStrLn $ title ++ " is not ours!"
        return vl

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  mainloop $ M.fromList []

mainloop :: Movies -> IO ()
mainloop vl = do
    menu
    c <- getChar
    putStr "\n"
    vl' <- case c of
       'g' -> do fmap snd $ rentVideo vl
       'r' -> do returnVideo vl
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

