-- Matrix Module 
--
-- This stores all the basics for the implementation
-- 
-- Jacob Buete 2014

{-# LANGUAGE FlexibleInstances, OverlappingInstances, IncoherentInstances #-}

module Matrices (
    Matrix,
    Row,
    Column,
    Vector,
    singleton_matrix, -- :: Float -> Matrix
    identity_matrix,  -- :: Integer -> Matrix
    is_empty_matrix,  -- :: Matrix -> Bool
) where


import Text.Printf (printf)

type Row     = [Float]

type Column  = [Float]

type Vector  = [Float]

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
                x:xs  -> "| " ++ show_rows x ++ " |\n" ++ show_matrix xs

instance Show (Vector) where
    show vector = show_vector vector
        where
            show_vect_elems :: Float -> String
            show_vect_elems element = "| " ++ printf "%-3.2f" element ++ " |"

            show_vector :: Vector -> String
            show_vector vector = case vector of
                []    -> ""
                [x]   -> show_vect_elems x
                x:xs  -> show_vect_elems x ++ "\n" ++ show_vector xs


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


is_empty_matrix :: Matrix -> Bool
is_empty_matrix matrix = case matrix of 
    []   -> True 
    x:xs -> case x of 
                [] -> True && is_empty_matrix xs
                _  -> False