## These functions find the inverse of a matrix and store both the matrix
## and the result.

## The first function, makeCacheMatrix(), creates an R object that stores a
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  v <- NULL
	  set <- function(y) {
			x <<- y
			v <<- NULL
	  }
	  get <- function() x
	  setinv <- function(inverse) v <<- inverse
	  getinv <- function() v
	  list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## The second function, cacheSolve(), requires an argument that is returned
## by makeCacheMatrix() in order to retrieve the inverse from the cached
## value that is stored in the makeCacheMatrix() object's environment.
## If there is no cached value, it takes x (using the get function from the
## list) and inverts x. It then sets the inverse of the matrix into the
## object so that it can recall it if it requires it another time.

cacheSolve <- function(x, ...) {
 	  v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}

