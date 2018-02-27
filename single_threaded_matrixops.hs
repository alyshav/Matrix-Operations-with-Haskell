-- Functional Programming Implementation
-- Linear Matrix Operations
-- by Alysha van Duynhoven

import Data.List
import Data.Ord
import System.CPUTime
import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time
import System.Environment

-- Multiply Matrices function
multmatrices :: (Num a) => [[a]] -> [[a]] -> [[a]] 
multmatrices a b = [[ sum $ zipWith (*) c d | d <- (transpose b) ] | c <- a ]

-- Add Matrices function
addmatrices :: Num a => [[a]] -> [[a]] -> [[a]] 
addmatrices a b = zipWith (zipWith (+)) a b

-- Obtain the max(aij, bij) terms as a matrix
getmax :: (Ord a, Num a) => [[a]] -> [[a]] -> [[a]]
getmax a b = zipWith (zipWith (max)) a b

-- Obtain the aij^2 or bij^2 terms as a matrix
squared :: Num a => [[a]] -> [[a]]
squared a = zipWith (zipWith (*)) a a

-- Dense & Sparse Matrices Generation For Testing
createdensematrix n = [[2*x | x <- [1..n]] | x <- [1..n]]
createsparsematrix n = [[if x==1 then 1 else 0| x <- [1..n]] | x <- [1..n]]

-- Run the steps to perform the custom matrix operation
customfunction a b = do
        let f = squared a
        let g = squared b
        let e = getmax a b
        let h = addmatrices f g
        let i = addmatrices h e
        print "Completed Custom Matrix Operation"
        print i
-- Run the multiply matrix operation
multfunction a b = do
        let i = multmatrices a b
        print "Completed Multiplication Operation"
        print i
-- Run the add matrix operation
addfunction a b = do
        let i = addmatrices a b
        print "Completed Add Matrix Operation"
        print i
-- Testing
main :: IO()
main = do
        let size = 1000 -- change matrix dims here
        let a = createdensematrix size
        let b = createdensematrix size
        customfunction a b -- change function here
        return ()