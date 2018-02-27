-- Functional Programming Implementation
-- Parallelized Matrix Operations
-- by Alysha van Duynhoven

import Data.List
import Data.Ord
import System.CPUTime
import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies

-- Parallelized Wrapper: Accepts 2 matrices to evaluate before applying the function 'f'
-- The function 'f' in this implementation typically refers to the concat function, except in an instance in the custom function implementation
parwrapper :: (a -> b -> c) -> (a -> b -> c)
parwrapper f x y = x `par` y `par` f x y

-- Multiply Matrices function obtains the product of two matrices
multmatrices :: (Num a) => [[a]] -> [[a]] -> [[a]] 
multmatrices a b = [[ sum $ zipWith (*) c d | d <- (transpose b) ] | c <- a ]

-- Add Matrices function obtains the summation of two matrices
addmatrices :: Num a => [[a]] -> [[a]] -> [[a]]
addmatrices a b = zipWith (zipWith (+)) a b

-- Obtain the max(aij, bij) terms as a matrix
getmax :: (Ord a, Num a) => [[a]] -> [[a]] -> [[a]]
getmax a b = zipWith (zipWith (max)) a b

-- Obtain the aij^2 or bij^2 terms as a matrix
squared :: Num a => [[a]] -> [[a]]
squared a = zipWith (zipWith (*)) a a

-- Dense & Sparse Matrix Generation For Testing
createdensematrix n = [[2*x | x <- [1..n]] | x <- [1..n]]
createsparsematrix n = [[if x==1 then 1 else 0| x <- [1..n]] | x <- [1..n]]
-- Testing matrices
densemat10 = createdensematrix 10
sparsemat10 = createsparsematrix 10

--concatenate 2 matrices
concat1 :: [a] -> [a] -> [a]
concat1 x y = x ++ y

-- Run the steps to perform the custom matrix operation
customfunction a b = do
        let f = squared a
        let g = squared b
        let e = getmax a b
        let h = addmatrices f g
        let i = addmatrices h e
        print "Completed Custom Matrix Operation"
-- Run the steps to perform the parallelized custom matrix operation
parcustomfunction a b d = do
        let v = squared a
        let w = squared b
        let g = getmax v w
        let x = addmatrices (take d v) (take d w)
        let y = addmatrices (reverse (take d (reverse v))) (reverse (take d (reverse w)))
        let j = addmatrices (take d x) (take d g)
        let k = addmatrices (reverse (take d (reverse x))) (reverse (take d (reverse g)))
        let l = concat1 j k
        let i = parwrapper concat1 l y
        print i
        print "Completed Custom Matrix Operation with Parallelized matrix evaluation"
-- Run the multiply matrix operation
multfunction a b = do
        let i = multmatrices a b
        print i
        print "Completed Multiplication Operation"
-- Run the parallelized multiply matrix operation
parmultfunction a b d = do
        let x = multmatrices (take d a) (take d b)
        let y = multmatrices (reverse (take d (reverse a))) (reverse (take d (reverse b)))
        let i = parwrapper concat1 x y
        print i
        print "Completed Parallelized Multiplication Operation"
-- Run the add matrix operation
addfunction a b = do
        let i = addmatrices a b
        print "Completed Add Matrix Operation"
-- Run the parallelized add matrix operation
paraddfunction a b d = do
        let x = addmatrices (take d a) (take d b)
        let y = addmatrices (reverse (take d (reverse a))) (reverse (take d (reverse b)))
        let i = parwrapper concat1 x y
        print i
        print "Completed Parallelized Add Matrix Operation"
-- Testing
main :: IO()
main = do
        let size = 1000
        let division = 500 -- size/2
        let a = createdensematrix size
        let b = createdensematrix size
        parmultfunction a b division
        --multfunction a b
        return ()