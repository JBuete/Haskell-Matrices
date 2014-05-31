-- Jacob Buete 2014
--
-- An Implementation of Matrices in Haskell
-- A beta

module Algebra (
    add_matrices,          -- :: Matrix -> Matrix -> Matrix
    sub_matrices,          -- :: Matrix -> Matrix -> Matrix
    scale_matrix,          -- :: Float -> Matrix -> Matrix
    transpose_matrix,      -- :: Matrix -> Matrix
    apply_vector,          -- :: Matrix -> Vector -> Vector
    matrix_multiplication, -- :: Matrix -> Matrix -> Matrix
) where

import Matrices

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

scale_matrix :: Float -> Matrix -> Matrix
scale_matrix n matrix = case matrix of 
    []   -> []
    x:xs -> (map (* n) x):scale_matrix n xs 

transpose_matrix :: Matrix -> Matrix
transpose_matrix matrix 
    | is_empty_matrix matrix = []
    | otherwise              = (generate_column matrix):transpose_matrix (map (tail) matrix)

generate_column :: Matrix -> Column
generate_column matrix = case matrix of 
    [[]]   -> []
    x:xs ->map (head) matrix

apply_vector :: Matrix -> Vector -> Vector
apply_vector matrix vector = map (row_mult vector) matrix
                                where 
                                    tuple_mult :: (Num a) => (a,a) -> a
                                    tuple_mult tuple = (fst tuple) * (snd tuple)

                                    row_mult :: Vector -> Row -> Float
                                    row_mult vector row = sum (map (tuple_mult) (zip  vector row))


matrix_multiplication :: Matrix -> Matrix -> Matrix
matrix_multiplication matrix_a matrix_b = transpose_matrix (map (apply_vector matrix_a) (transpose_matrix matrix_b))


