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
    det_two,               -- :: Matrix -> Float
    cofactor_matrix,       -- :: Matrix -> Matrix
    adjoint_matrix,        -- :: Matrix -> Matrix
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


remove_at :: Int -> [a] -> [a]
remove_at k list = case list of 
    []              -> error "List not that long"
    x:xs 
        | k == 1    -> xs
        | otherwise -> x:remove_at (k-1) xs

find_cofactor :: Int -> Int -> Matrix -> Matrix
find_cofactor n m matrix = map (remove_at m) (remove_at n matrix)

det_two :: Matrix -> Float
det_two matrix = (head top)*(last bottom) - (head bottom)*(last top)
                    where 
                        top    = head matrix
                        bottom = last matrix

cofactor_matrix :: Matrix -> Matrix
cofactor_matrix matrix = cofactor_aux (length matrix) (length (head matrix))
                            where 
                                cofactor_row :: Int -> Int -> Row
                                cofactor_row n m
                                    | m == 0             = []
                                    | (m+n) `mod` 2 == 1 = cofactor_row n (m-1) ++ [-det_two (find_cofactor n m matrix)]
                                    | otherwise          = cofactor_row n (m-1) ++ [det_two (find_cofactor n m matrix)]

                                cofactor_aux :: Int -> Int -> Matrix
                                cofactor_aux n m 
                                    | n == 0    = []
                                    | otherwise = cofactor_aux (n-1) m ++ [cofactor_row n m]

adjoint_matrix :: Matrix -> Matrix
adjoint_matrix matrix = transpose_matrix (cofactor_matrix matrix)