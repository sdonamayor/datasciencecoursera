###CACHING THE INVERSE OF A MATRIX###


## 1. Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {

	inv <- NULL

	set <- function(matrix) {
			m <<- matrix
			inv <<- NULL
	}

	get <- function() m

	setInv <- function(inverse) {
		inv <<- inverse
	}

	getInv <- function() inv

	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## 2. Computes the inverse of the special matrix returned by "makeCacheMatrix". If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	m <- x$getInverse()

	if( !is.null(m) ) {
			message("Cached inverse matrix")
			return(m)
	}

	data <- x$get()

	m<-solve(data, ...)

	x$setInverse(m)

	m
}
