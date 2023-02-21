{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( findPuzzle, GeomData
    ) where

import Data.List
import Control.Monad

-- holds the geometric data defining a puzzle
-- (diagonal value of (A A^T)^-1, off-diagonal value of (A A^T)^-1, gamma^2)
type GeomData a = (a, a, a)

findPuzzle :: forall a . (Fractional a, Ord a) => GeomData a -> Int -> Maybe [a]
findPuzzle (diag, offDiag, cutBound2) n = checkVoxel $ replicate ((n-1)`div`2) [0, 1]
    where
        -- gets the [i,j,k] for all centers with -to ≤ i,j,k ≤ to and i+j+k = by
        centersToBy :: Int -> Int -> [[Int]]
        centersToBy to by = [ [i, j, k] | i <- list, j <- list, let k = by-i-j, k `elem` list, i <= j && j <= k ]
            where list = [-to,-to+2..to]

        centers :: [[Int]]
        centers = centersToBy (n-1) (n+1) ++ centersToBy (n-3) (n-1)

        genIneqsF :: Int -> [[Int]]
        genIneqsF k = filter (n `notElem`) $ fmap (fmap (+k)) centers

        ineqsFI :: [[Int]]
        ineqsFI = genIneqsF (-1)

        ineqsFO :: [[Int]]
        ineqsFO = genIneqsF 1

        -- this is the operator xs |-> xs^T (A A^T)^-1 xs, implemented without matrices
        bilinear :: [a] -> a -- Fractional a => 
        bilinear xs = (offDiag)*(sum xs)^(2::Int) + (diag-offDiag)*sum ((^(2::Int)) <$> xs)

        -- gets the cut depth for any index, including zero and negative
        cutD :: [a] -> Int -> a -- Fractional a => 
        cutD _ 0 = 0
        cutD pt i
            | i > 0 = pt !! ((i-1) `div` 2)
            | i < 0 = -(pt !! ((-i-1) `div` 2))

        evalIneq :: [a] -> [Int] -> [a] -- Fractional a => 
        evalIneq pt = fmap (cutD pt)

        -- checks each inequalitiy on a given vertex
        -- (<<) can be (<) for strict inequalities or (<=) for non-strict ones
        -- it is safer to check non-strict inequalities when checking if a box has no solutions
        checkVertex :: (a -> a -> Bool) -> [a] -> [Bool] -- (Fractional a) => 
        checkVertex (<<) pt = satFI ++ satFO ++ satDM ++ satV
            where satFI = zipWith (||) (((<<1) . bilinear . evalIneq pt) <$> ineqsFI) (((<<0) . sum . evalIneq pt) <$> ineqsFI) 
                  satFO = ((1<<) . bilinear . evalIneq pt) <$> ineqsFO
                  satDM = zipWith (<<) (0 : init pt) pt
                  satV  = [((last pt)^(2::Int)) << cutBound2]

        -- checks the inequalities on every vertex and either
        -- returns (Just solution), Nothing, or splits the box and recurses
        checkVoxel :: [[a]] -> Maybe [a] -- (Fractional a, Ord a) => 
        checkVoxel bounds -- bounds is [[lower,upper],[lower,upper],...]
            | not $ and $ fmap or $ transpose ineqs = Nothing
            | or $ fmap and $ ineqs = case find snd $ zip verts $ fmap and $ ineqsS of
                    Just (v,_) -> Just v
                    Nothing    -> recurse
            | otherwise = recurse
            where verts = sequence $ bounds -- sequence is cartesian product
                  ineqs = checkVertex (<=) <$> verts -- non-strict to check if there's a solution in the bounds
                  ineqsS = checkVertex (<) <$> verts -- strict to check if there's a solution on a vertex
                  newBounds = sequence $ (\[a,b] -> (\l -> transpose [init l, tail l]) $ take (2+1) $ iterate (+(b-a)/2) a) <$> bounds
                  recurse = msum $ checkVoxel <$> newBounds -- msum gets the first Just




