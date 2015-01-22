## makeCacheMatrix creates a cache structure
## cacheSolve uses makeCacheMatrix to either retrieve
## the inverse from cache or compute it if it is not cached

## makeCacheMatrix takes a matrix as input and creates a list of functions
## to help with getting or changing the matrix as well as
## getting and setting the inverse. Used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve tries to get the stored value of inv
## if it is NULL, then it calculates the inverse using solve()
## and caches it for further use

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
