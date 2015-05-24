## The matrix inversion is a costly computation and its better to caching it results instead of constantly computing it. The following two functions are usted to cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get,
	     setinverse=setinverse, 
	     getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special matrix return by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

## Functional Tests
## Create a 5 rows, 5 columns matrix from normal distribution values
mat = matrix(rnorm(1000), nrow=5, ncol=5)
## Execute the makeCacheMatrix
m = makeCacheMatrix(mat)
m$get()
## Expect first computation
cacheSolve(m)
## Expect the cached data
cacheSolve(m)
