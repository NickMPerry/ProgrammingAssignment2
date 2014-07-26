## makeCacheMatrix implements a CacheMatrix object: essentially a special 
## matrix "wrapped" in a list, which can then cache both the matrix itself 
## and its inverse.
## cacheSolve then uses the CacheMatrix to solve for its inverse, saving time
## by consulting a previously cached value if possible, or computing and
## cacheing the inverse if necessary

## Creates a matrix "wrapped" in a list that allows for the cacheing of the matrix
## and its inverse -- values can then be looked up using the wrapper at any
## time without further computation 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 		## initializes a variable for inverse
	
	set <- function(new) { 	## sets matrix and resets inverse in wrapper
		x <<- new
		inverse <<- NULL
	}
	get <- function() x		## returns matrix from wrapper
	setinverse <- function(newinverse) inverse <<- inverse
							## sets inverse of matrix in wrapper
	getinverse <- function() inverse 
							## returns inverse of matrix from wrapper						
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
							## returns "wrapper" containing CacheMatrix object
							## including all of above functionality
}


## Calculates the inverse of a matrix wrapped using the above function,
## checking first for an already cached inverse. If none exists, does a new
## computation, then caches and returns the result.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {			## checks for already cached inverse
        	message("getting cached data")
        	return(inverse)				## cached inverse found; return it!
        }
        matrix <- x$get()
        newinverse <- solve(matrix)		## no cached inverse found; compute it!
        x$setinverse(newinverse)		## then cache it
        newinverse						## then return it
}
