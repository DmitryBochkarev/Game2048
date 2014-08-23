module Game2048 
  (
    Direction(..),
    Matrix,
    initMatrix,
    move
  )
  where

import System.Random (randomRIO)
import Control.Monad (replicateM, forM)

data Direction = DLeft | DRight | DUp | DDown
                 deriving (Show)
                
type Matrix = [[Int]]

initMatrix :: Int -> IO Matrix
initMatrix size = do
    replicateM size $ replicateM size (initCell (size * 2))

move :: Direction -> Matrix -> IO Matrix
move direction matrix = 
    let matrix' = shift direction matrix
    in if matrix' == matrix
         then return matrix'
         else initFreeCells matrix'

initFreeCells :: Matrix -> IO Matrix
initFreeCells matrix = do
    forM matrix $ mapM (initCellIfFree totalFreeCells)
  where
    totalFreeCells = countFreeCells matrix

initCellIfFree :: Int -> Int -> IO Int
initCellIfFree totalFree 0   = initCell totalFree
initCellIfFree _         val = return val

initCell :: Int -> IO Int
initCell totalFree = do
    v <- randomRIO (0, totalFree)
    return $ if v == totalFree
               then 2
               else 0

countFreeCells :: Matrix -> Int
countFreeCells matrix =
    foldl (+) 0 $ map (foldl plusOneIfZero 0) matrix
  where
    plusOneIfZero :: Int -> Int -> Int
    plusOneIfZero sum 0 = sum + 1
    plusOneIfZero sum _ = sum

shift :: Direction -> Matrix -> Matrix
shift DLeft matrix  = map shiftRow matrix
shift DRight matrix = map shiftRowReverse matrix
shift DUp matrix    = transpose $ map shiftRow $ transpose matrix
shift DDown matrix  = transpose $ map shiftRowReverse $ transpose matrix

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

shiftRow :: [Int] -> [Int]
shiftRow row =
    shiftRow' (normalizeRow row)
  where
    shiftRow' []         = []
    shiftRow' [x]        = [x]
    shiftRow' (a:b:tail) =
      let (a', b') = merge (a, b)
      in normalizeRow $ a' : shiftRow' (b':tail)

shiftRowReverse :: [Int] -> [Int]
shiftRowReverse = reverse . shiftRow . reverse

normalizeRow :: [Int] -> [Int]
normalizeRow row = normalizeRow' 0 row
  where
    normalizeRow' :: Int -> [Int] -> [Int]
    normalizeRow' _        []     = []
    normalizeRow' tailSize [x]    = x : replicate tailSize 0
    normalizeRow' tailSize (0:xs) = normalizeRow' (tailSize + 1) xs
    normalizeRow' tailSize (x:xs) = x : normalizeRow' tailSize xs

merge :: (Int, Int) -> (Int, Int)
merge (a, b) | a == b    = (a + b, 0)
             | otherwise = (a, b)
