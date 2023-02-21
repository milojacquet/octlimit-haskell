module Main (main) where

import Lib
import QSqrt5
import Data.Ratio
import System.Environment

octa :: GeomData Rational
octa = (3/2, 3/4, 1/3)

icosa :: GeomData QSqrt5
icosa = (phi+2, QSqrt5 (5/4) (3/4), 1/5)


main :: IO ()
main = do
    [puzzleStr, orderStr] <- getArgs
    let order = read orderStr
    let output | puzzleStr == "octa"  = show $ (findPuzzle octa  order)
               | puzzleStr == "icosa" = show $ (findPuzzle icosa order)
    putStrLn output
