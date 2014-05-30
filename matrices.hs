-- Jacob Buete 2014
--
-- An Implementation of Matrices in Haskell
-- A beta

module Main (
	singleton_matrix, -- :: Element -> Matrix
	add_matrices,     -- :: Matrix -> Matrix -> Matrix
) where

--data Element = Float
	--deriving (Eq, Ord, Enum)


type Row     = [Float]


type Matrix  = [Row]

singleton_matrix :: Float -> Matrix
singleton_matrix a = [[a]]


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

