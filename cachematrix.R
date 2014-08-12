## Overview:
### Caching the inverse of a matrix using lexical scoping mechanism
### It's the deliverable for programming assignment 2 of R Programming.

## Description:
### Creates a "matrix" object which can cache the inverse of itself.
## Arguments:
### x	The original matrix object(should be invertible), the 
### default value is a blank matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL

	## set the original matrix(should be invertible)
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}	
	
	## get the original matrix
	get <- function() x
	
	## set the inverse to cache
	setInverse <- function(i) inv_matrix <<- i
	
	## get the inverse matrix from cache
	getInverse <- function() inv_matrix
	
	## help information, list the inner functions
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## Description:
### Get the inverse of the "matrix" 
### Retrieve the result from cache if the matrix has not changed 
### and have been calculated before, or calculate it and set to cache 
## Arguments:
### x A function object of makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv_m <- x$getInverse()
	if(!is.null(inv_m)) {
		message("getting cached data")
		return(inv_m)
	}
	
	data <- x$get()
	inv_m <- solve(data, ...)
	x$setInverse(inv_m)
	
	inv_m
}

## The test case - run testCache() in R Console.
### passed the test with expected result as the following:
### Give "getting cached data" when running for the first time,
### Don't give the above message when running aferwards.

testCache <- function(){
	mt <- matrix(c(0,1,3,3,-1,-1,1,1,2),3,3)
	print("original matrix:")
	print(mt)
	
	cached_mt <- makeCacheMatrix(mt)
	cached_inv <- cacheSolve(cached_mt)

	print("inverse matrix:")
	cached_inv	
}
