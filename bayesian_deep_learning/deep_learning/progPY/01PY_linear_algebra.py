
######################################################################
# PROGRAM: 01 - linear algebra
# DATE:    2018-04-04
# NOTE:
######################################################################

import numpy as np

## Basics
# vector
x = np.array([1,2,3,4])
x

# matrix
A = np.array([[1, 2], [3, 4], [5, 6]])
A
A.shape # shape
x.shape

len(x)

## Transposed Matrix
# The transpose AT of the matrix A corresponds to the mirrored axes.
# If the matrix is a square matrix (same number of columns and rows)
# If the matrix is not square the idea is the same:
# The shape (m×n) is inverted and becomes (n×m)

A_t = A.T
A_t

A.shape
A_t.shape

## Addition
# matrices can be added if they have the same shape
B = np.array([[2, 5], [7, 4], [4, 3]])
B

# add matrices A and B
C = A + B
# add a scalar
C = A+4
C

## Broadcasting
# Numpy can handle operations on arrays of different shapes. 
# The smaller array will be extended to match the shape of the bigger one. 
# The advantage is that this is done in C under the hood 
# (like any vectorized operations in Numpy). 
# Actually, we used broadcasting in the example 5. 
# The scalar was converted in an array of same shape as A.

A = np.array([[1, 2], [3, 4], [5, 6]])
B = np.array([[2], [4], [6]])
# Broadcasting
C=A+B
C

# Multiplication
# The matrix product, also called dot product (different from the element-wise product):
A = np.array([[1, 2], [3, 4], [5, 6]])
B = np.array([[2], [4]])
C = np.dot(A, B)
C
C = A.dot(B) # method
C

# matrix x matrix
A = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]])
B = np.array([[2, 7], [1, 2], [3, 6]])
C = A.dot(B)
C

## Properties of the dot product
# Distributive
A = np.array([[2, 3], [1, 4], [7, 6]])
B = np.array([[5], [2]])
C = np.array([[4], [3]])


# A(B+C)
D = A.dot(B+C)
D

# is equivalent to AB+AC:
D = A.dot(B) + A.dot(C)
D

# Associative
A = np.array([[2, 3], [1, 4], [7, 6]])
B = np.array([[5, 3], [2, 2]])
D = A.dot(B.dot(C))
D

D = (A.dot(B)).dot(C)
D

# Not Commutative
A = np.array([[2, 3], [6, 5]]); A
B = np.array([[5, 3], [2, 2]]); B
AB = np.dot(A, B)
AB
BA = np.dot(B, A)
BA

## System of linear equations
# Matrices can be used to describe a system of linear equations of the form Ax=b
# A system of equations is defined by its number of equations and its number of unknowns. 
# In our example above, the system has 2 equations and 2 unknowns (x and y).
# In addition we call this a system of linear equations because each equations is linear. 
# It is easy to see that in 2 dimensions: 
# we will have one straight line per equation and the dimensions are the unknowns. 

## Identity matrix
np.eye(3)
# when ‘apply’ the identity matrix to a vector the result is this same vector

x = np.array([[2], [6], [3]])
xid = np.eye(x.shape[0]).dot(x)
xid

# You can think of a matrix as a way to transform objects in a n-dimensional space. 
# It applies a linear transformation of the space.
A = np.array([[3, 0, 2], [2, 0, -2], [0, 1, 1]])
A
A_inv = np.linalg.inv(A)
A_inv

# We must note however that non square matrices 
# (matrices with more columns than rows or more rows than columns) don’t have inverse.
# the inverse and the identity matrix allow us to solve linear system of equations
A = np.array([[2, -1], [1, 1]])
A_inv = np.linalg.inv(A)
b = np.array([[0], [3]])
x = A_inv.dot(b)
x

# RK = Some matrices are not invertible. They are called singular.

# plot solution
import matplotlib.pyplot as plt
x = np.arange(-10, 10)
y = 2*x
y1 = -x + 3

plt.figure()
plt.plot(x, y)
plt.plot(x, y1)
plt.xlim(0, 3)
plt.ylim(0, 3)
# draw axes
plt.axvline(x=0, color='grey')
plt.axhline(y=0, color='grey')
plt.show()
plt.close()
























    
    