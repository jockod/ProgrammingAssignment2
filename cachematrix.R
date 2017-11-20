## I've wrapped the two assignment functions into a parent, to make testing
## and demonstrating easier.  genInvMatrix() just calls makeCacheMatrix() and
## then cacheSolve() to present the solution inverse matrix.

genInvMatrix <- function(x = matrix()) {
        testMatrix <- makeCacheMatrix(x)
        solutionMatrix <- cacheSolve(testMatrix)
        return(solutionMatrix)
}

## makeCacheMatrix() takes a matrix argument and generates creates an external
## version that also includes the ability to set and get matrix value
## and to fetch or create inverses

makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL

        ## set function sets the value of the external matrix and puts a null in for the
        ## value of the inverse
	set <- function (y) {
		x <<- y
		matInv <<- NULL
	}
	
        ## get function retrieves the value of the matrix
	
	get <- function() x

	## setInverse than does a similar operation, setting a global matrix
	## "matInv" containing the value of the inverse of the matrix using the
	## solve() function
	
	setInverse <- function(solve) matInv <<- solve

	## getInverse retrieves the resulting inverse matrix
	
	getInverse <- function() matInv
	
	## the list function then creates the return vector with the
	## functions names within it
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve either generates and stores, or retrieves, the inverse of the
## matrix passed to it.
cacheSolve <- function(x, ...) {
        matInv <- x$getInverse()
	if(!is.null(matInv)) {
		message("getting cached data")
		return(matInv)
	}

        data <- x$get()
        matInv <- solve(data)

	x$setInverse(matInv)

	matInv
}
