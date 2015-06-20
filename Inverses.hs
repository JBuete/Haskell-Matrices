-- Inverses Module 
--
-- This stores all the fundamental things for manipulating matrices
-- This includes things like determinants and inverse matrices
-- 
-- Jacob Buete 2014

module Inverses (
	det_two,         -- :: Matrix -> Float
    determinant,     -- :: Matrix -> Float
    cofactor_matrix, -- :: Matrix -> Matrix
    adjoint_matrix,  -- :: Matrix -> Matrix
    inverse_matrix,  -- :: Matrix -> Matrix
) where

import Matrices
import Algebra 



remove_at :: Int -> [a] -> [a]
remove_at k list = case list of 
    []              -> error "List not that long"
    x:xs 
        | k == 1    -> xs
        | otherwise -> x:remove_at (k-1) xs

find_cofactor :: Int -> Int -> Matrix -> Matrix
find_cofactor n m matrix = map (remove_at m)  (remove_at n matrix)

det_two :: Matrix -> Float
det_two matrix = (head top)*(last bottom) - (head bottom)*(last top)
                    where 
                        top    = head matrix
                        bottom = last matrix

element_at :: Int -> Row -> Float
element_at k row = case row of 
    []   -> error "Nope"
    x:xs 
        | k == 1    -> x
        | otherwise -> element_at (k-1) xs

determinant :: Matrix -> Float
determinant matrix 
    | length matrix == 2  = det_two matrix
    | is_singleton matrix = head (head matrix)
    | otherwise           = find_det (length matrix) matrix
                            where
                                det_element :: Int -> Matrix -> Float
                                det_element m matrix 
                                    | m`mod`2 == 0 = (-element_at m (head matrix)) * determinant (find_cofactor 1 m matrix)
                                    | otherwise    = (element_at m (head matrix)) * determinant (find_cofactor 1 m matrix)

                                find_det :: Int -> Matrix -> Float
                                find_det m matrix 
                                    | m == 0    = 0
                                    | otherwise = (det_element m matrix) + find_det (m-1) matrix


cofactor_matrix :: Matrix -> Matrix
cofactor_matrix matrix = cofactor_aux (length matrix) (length (head matrix))
                            where 
                                cofactor_row :: Int -> Int -> Row
                                cofactor_row n m
                                    | m == 0             = []
                                    | (m+n) `mod` 2 == 1 = cofactor_row n (m-1) ++ [-determinant (find_cofactor n m matrix)]
                                    | otherwise          = cofactor_row n (m-1) ++ [determinant (find_cofactor n m matrix)]

                                cofactor_aux :: Int -> Int -> Matrix
                                cofactor_aux n m 
                                    | n == 0    = []
                                    | otherwise = cofactor_aux (n-1) m ++ [cofactor_row n m]

adjoint_matrix :: Matrix -> Matrix
adjoint_matrix matrix = transpose_matrix (cofactor_matrix matrix)

inverse_matrix :: Matrix -> Matrix
inverse_matrix matrix 
	| is_singleton matrix = [[(1/head (head matrix))]]
	| otherwise           = scale_matrix (1/(determinant matrix)) (adjoint_matrix matrix)




