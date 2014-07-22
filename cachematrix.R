## This file contains two functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix can (but need not) take a matrix as argument. It creates a 
## special object that behaves like a class. The return object is a list of 
## functions to interact with the class instance: 
## get() retrieves the matrix. 
## set(y) re-initializes to a new matrix (and resets the inverse to NULL ie. unknown). 
## setinv(y) saves a matrix x as the inverse, and should not be used directly by the user.
## getinv() returns the inverse (NULL if unknown), should also not be used directly.
## The fuctions for the inverse can be accessed through cacheSolve.
##
## cacheSolve takes an object of the type returned by makeCacheMatrix, and returns
## the inverse (and saves it if it was NULL before). Faster than solve for repeated use,
## especially on large matrices, because the saved inverse is returned if it exists.


## Creates a class-like matrix object, returning a list of functions to interact with the 
## matrix: 
## get(), set(y), getinv() and setinv(y), retrieving and setting the matrix and its inverse,
## respectively

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setinv <- function(y)
    {
        inv <<- y
    }
    
    getinv <- function()
    {
        inv
    }
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse matrix of a makeCacheMatrix object: returns the stored version
## if it exists, calculates and stores otherwise.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinv()
    if(is.null(inv))
    {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    }
    inv
}
