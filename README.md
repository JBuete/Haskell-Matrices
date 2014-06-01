#Haskell Matrices
====================
A Matrix Module for Haskell!

##Description
This module aims to contain everything needed in order to perform the majority of applications of Linear Algebra in a module for the Haskell Programming Language. 

##What can you do?
With Haskell Matrices you are able to do a lot of the basic matrix manipulation and analysis that are covered in introductory Linear Algebra Couses. There are plans to expand this further in the future to allow for solving matrix equations and finding eigenvalues/eigenvectors and various canonical forms. 

###Input
When working from the terminal you can define a matrix as the following 

	[[1,2],[3,4]]
	
With each of the internal lists representing an individual row. 

Vectors are defined within the terminal as 

	[1,2,3]
	
###Functions
Within the modules you have access to multiple ways to interact with matrices and vectors. These include

	identity_matrix :: Integer -> Matrix
	is_empty_matrix :: Matrix -> Bool
	
	add_matrices :: Matrix -> Matrix -> Matrix 
	sub_matrices :: Matrix -> Matrix -> Matrix 
	scale_matrix :: Float -> Matrix -> Matrix 
	transpose_matrix :: Matrix -> Matrix
	apply_vector :: Matrix -> Vector -> Vector 
	matrix_multiplication :: Matrix -> Matrix -> Matrix
	
	determinant :: Matrix -> Float
	cofactor_matrix :: Matrix -> Matrix
	adjoint_matrix :: Matrix -> Matrix 
	inverse_matrix :: Matrix -> Matrix
	

##Licensing 
All relevant licensing information and copyright can be found in the */LICENSE* [file](http://github.com/JBuete/Haskell-Matrices/blob/master/LICENSE) in the Github Repository. 