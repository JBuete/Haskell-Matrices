-- Jacob Buete 2014
--
-- An Implementation of Matrices in Haskell
-- A beta

{-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-}

module Main (
    Matrix, 
    singleton_matrix, -- :: Element -> Matrix
    identity_matrix,  -- :: Integer -> Matrix
    add_matrices,     -- :: Matrix -> Matrix -> Matrix
    sub_matrices,     -- :: Matrix -> Matrix -> Matrix
) where

import Text.Printf (printf)

type Row     = [Float]

type Matrix  = [Row]

instance Show (Matrix) where
    show matrix = show_matrix matrix
        where
            show_element :: Float -> String
            show_element element = printf "%-3.2f" element

            show_rows :: Row -> String
            show_rows row = case row of 
                []    -> ""
                [x]   -> show_element x
                x:xs  -> (show_element x) ++ "  " ++ show_rows xs

            show_matrix :: Matrix -> String
            show_matrix matrix = case matrix of 
                []    -> ""
                [x]   -> "| " ++ show_rows x ++ " |"
                x:xs  -> "| " ++ (show_rows x)  ++ " |\n" ++ show_matrix xs


singleton_matrix :: Float -> Matrix
singleton_matrix a = [[a]]

insert_at :: Integer -> Float -> Row -> Row
insert_at k element row 
    | k == 1    = element:(tail row)
    | otherwise = first :insert_at (k-1) element rest
                    where
                        first = head row
                        rest  = tail row

zero_row :: Integer -> Row
zero_row n 
    | n == 0    = []
    | otherwise = 0:zero_row (n-1)

identity_matrix :: Integer -> Matrix
identity_matrix n = construct n []
                        where
                            construct :: Integer -> Matrix -> Matrix 
                            construct dimension matrix  
                                | dimension == 0 = matrix
                                | otherwise      = construct (dimension - 1) ((insert_at dimension 1 (zero_row n)):matrix)

add_matrices :: Matrix -> Matrix -> Matrix
add_matrices matrix_a matrix_b = compare_matrices (+) matrix_a matrix_b

sub_matrices :: Matrix -> Matrix -> Matrix
sub_matrices matrix_a matrix_b = compare_matrices (-) matrix_a matrix_b

compare_matrices :: (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix
compare_matrices comparison matrix_a matrix_b  = case (matrix_a, matrix_b) of 
    ([],[])     -> []
    ([],y:ys)   -> error "Your matrices can't be add together"
    (x:xs,[])   -> error "Your matrices can't be add together"
    (x:xs,y:ys) -> (compare_rows x y): compare_matrices comparison xs ys
                    where 
                        compare_rows :: Row -> Row -> Row
                        compare_rows row_a row_b = case (row_a, row_b) of 
                            ([],[])     -> []
                            ([],y:ys)   -> error "nope"
                            (x:xs,[])   -> error "nope"
                            (x:xs,y:ys) -> (comparison x y):compare_rows xs ys 

