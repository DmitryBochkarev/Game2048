module Main where

import Game2048
import Data.Char
import System.IO
import Control.Monad (forM_)
import Text.Printf (printf)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    matrix <- initMatrix 4
    gameLoop matrix
    return ()
    
gameLoop :: Matrix -> IO (Maybe Matrix)
gameLoop matrix = do
    putChar '\n'
    printMatrix matrix
    key <- getChar
    matrix' <- return $ case key of
      'q' -> Nothing
      'a' -> Just (move DLeft matrix)
      'd' -> Just (move DRight matrix)
      'w' -> Just (move DUp matrix)
      's' -> Just (move DDown matrix)
      _   -> Just (return matrix)
    case matrix' of
      Nothing     -> return Nothing
      Just matrix -> do
        matrix >>= gameLoop

printMatrix :: Matrix -> IO ()
printMatrix matrix = do
    forM_ matrix printRow

printRow :: [Int] -> IO ()
printRow row = do
    print row'
  where
    row' = foldl (++) "" $ map (printf "%4d|") row

