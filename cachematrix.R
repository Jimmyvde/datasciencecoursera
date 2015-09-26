## matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. cachematrix.r contains a pair of functions that can do this

## makeCacheMatrix gives the user a list of commands that can be used
## to setup a matrix and store its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, 
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve is a function that gives the user the inverse of a matrix
## depending on if the inverse is already calculated it either returns
## the inverse from cache or calculates it through the "solve" function

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		print("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
