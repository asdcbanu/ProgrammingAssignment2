## Overall description - set the matrix and retrive the inverse either
## from cache or calculate the inverse

## This function sets and get the matrix, sets and gets the inverse

makeCacheMatrix <- function(x = matrix()) {
				m <- NULL
				set <- function(y){
					x <<- y
					m <<- NULL
				}
				get <- function() x
				setinverse <- function(inverse) m <<- inverse
				getinverse <- function() m
				list(set = set, get = get, 
					setinverse = setinverse,
					getinverse = getinverse)
}


## The following calculates the inverse of the matrix created wihtin
## the above function. If inverse is already calculated, it gets from
## the cache, otherwise calulcates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	message ("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
