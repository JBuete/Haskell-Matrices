-- Jacob Buete 2014
--
-- An Implementation of Matrices in Haskell
-- A beta

module Main (
    singleton_matrix, -- :: Element -> Matrix
    add_matrices,     -- :: Matrix -> Matrix -> Matrix
) where

type Row     = [Float]

type Matrix  = [Row]

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
add_matrices matrix_a matrix_b  = case (matrix_a, matrix_b) of 
    ([],[])     -> []
    ([],y:ys)   -> error "Your matrices can't be add together"
    (x:xs,[])   -> error "Your matrices can't be add together"
    (x:xs,y:ys) -> (add_rows x y): add_matrices xs ys
                    where 
                        add_rows :: Row -> Row -> Row
                        add_rows row_a row_b = case (row_a, row_b) of 
                            ([],[])     -> []
                            ([],y:ys)   -> error "nope"
                            (x:xs,[])   -> error "nope"
                            (x:xs,y:ys) -> (x+y):add_rows xs ys 
