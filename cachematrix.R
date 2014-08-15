# Submission for the programming assignment 2 of R Programming.

# Overview:
# 	Caching the inverse of a matrix using lexical scoping mechanism.


# Description:
# 	Creates a "matrix" object which can cache the inverse of itself.
# Arguments:
# 	x	The original matrix object(should be invertible), the 
# 		default value is a blank matrix.

makeCacheMatrix <- function(x = matrix()) {
	
	inv_matrix <- NULL

	# 1. set the original matrix(should be invertible)
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}	
	
	# 2. get the original matrix
	get <- function() x
	
	# 3. set the inverse to cache
	setInverse <- function(i) inv_matrix <<- i
	
	# 4. get the inverse matrix from cache
	getInverse <- function() inv_matrix
	
	# 5. help information, list the inner functions
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


# Description:
# 	Get the inverse of the "matrix" - Retrieve the result from cache if the 
#	matrix has not changed and have been calculated before, or calculate it 
#	and set to cache 
# Arguments:
# 	x A function object of makeCacheMatrix

cacheSolve <- function(x, ...) {
	
    # 1. get the inverse matrix from cache
	inv_m <- x$getInverse()
	if(!is.null(inv_m)) {
		message("getting cached data")
		return(inv_m)
	}
	
	# 2. solve the inverse matrix and set the cache, if the inverse 
	#	 have not been cached before.
	data <- x$get()
	inv_m <- solve(data, ...)
	x$setInverse(inv_m)
	
	# 3. return the inverse matrix
	inv_m
}

# Description:
#	The test case - run testCache() in R Console.
#	The expected output should be:
# 		- Just print the inverse matrix when calling cacheSolve for the 1st time.
#		- Print the inverse matrix, and give a "getting cached data" message when 
#		  running afterwards.

testCache <- function(){

	# 1. create an invertible matrix
	mt <- matrix(c(0,1,3,3,-1,-1,1,1,2),3,3)
	print("original matrix:")
	print(mt)
	
	# 2. create cached matrix
	cached_mt <- makeCacheMatrix(mt)
	
	# 3. using cache for first round
	cached_inv_r1 <- cacheSolve(cached_mt)
	print("inverse matrix(1st round):")
	cached_inv_r1
	
	# 4. using cache for first round
	#	 this round will print the message "getting cached data"
	cached_inv_r2 <- cacheSolve(cached_mt)
	print("inverse matrix(2nd round):")
	cached_inv_r2
}
