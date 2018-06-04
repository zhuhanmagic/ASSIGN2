## This function could create a special matrix object which can cache its inverse.

## It's always a costly computation and it may be better to do the matrix inverse caching instead of repeated computation.

makeCacheMatrix <- function(x = matrix()) {
		inversedata <- NULL
		set <- function(y) {
			x <<- y
			inversedata <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inversedata <<- inverse
		getinverse <- function() inversedata
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## This function computes the inverse of Matrix cached by the makeCacheMatrix function. If the inverse was calculated and the matrix didn't change, it would retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedata <- x$getinverse()
        if(!is.null(inversedata)) {
        		message("getting cached data")
        		return(inversedata)
        }
        finaldata <- x$get()
        inversedata <- solve(finaldata, ...)
        x$setinverse(inversedata)
        inversedata
}
