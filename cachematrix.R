## Pair of functions to store a matrix and its inverse.
## Only recalculates the inverse when the matrix changes. 
## Useful for use with looping functions where changes to the 
## matrix may not occur with every loop.


## makeCacheMatrix function
##
## A function containing a list of functions
## Stores the a matrix and its inverse.
## (Resets the inverse to null if a new matrix is input so 
##  cacheSolve() required to calculate and cache new inverse)
## Retrieve matrix using: a <- makeCacheMatrix()
##				  a$get()
## 	 and inverse using: a$getsolve()

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) inv <<- solve
	getsolve <- function() inv
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}


## cacheSolve function
##
## Returns the inverse of the matrix input into makeCacheMatrix()
##(Either the cached inverse or calculates and caches the new inverse)

cacheSolve <- function(x, ...) {
	inv <- x$getsolve()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setsolve(inv)
	inv
}
