## Description:
## ------------
## This code was created to complete Assignment 2 of the Introduction to 
## R Programming course on Coursera. This file comprises two functions which
## provide the ability to cache a result for inverting a matrix, which is 
## useful in improving performance if this operation is performed repeatedly
## on a large square matrix or large number of square matrices.

## Usage:
## ------
## For a given invertible square matrix x, the inverse of x is obtained by 
## calling: cachesolve(makeCacheMatrix(x)). The first time this is executed,
## R will compute the inverse of matrix x and return the result (while also 
## storing the result in memory). For executions, the result will be retrieved 
## from memory.


## makeCacheMatrix Description:
## ----------------------------
## This function takes an invertible square matrix as its input. The output of 
## this function is a list of accessor and mutator functions that compute the 
## inverse of the matrix and caches the result. This output is useable by the 
## cachesolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() { 
        x
    }
    setinverse <- function(solve) {
        m <<- solve 
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve Description:
## ----------------------------
## This function takes the output of makeCacheMatrix as its input,
## which is a list of functions that compute the inverse of a square matrix
## and stores the result in cache. This method checks if a result exists
## in cache and returns it if it does, otherwise computes the inverse afresh.
## The output of this function is the inverse of the matrix. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

