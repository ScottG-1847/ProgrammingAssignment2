## Function to create a makeCachMatrix object
## that can store and retrieve a matrix and its inverse
makeCacheMatrix <- function(theMatrix = matrix()) {
	## Initialize the stored inverse
	theInverse <- NULL
	
	## Set a new value for the stored matrix
	## and clear the stored inverse
		setMatrix <- function(mx) {
		theMatrix <<- mx
		theInverse <<- NULL
	}
	
	## Return the stored matrix
		getMatrix <- function() {
		theMatrix
	}
	
	## Set a new value for the stored inverse
		setInverse <- function(newInverse) {
		theInverse <<- newInverse
	}
	
	## Return the stored inverse
		getInverse <- function() {
		theInverse
	}
	
	## Return a list of methods avaiable in this function
		list(setMatrix = setMatrix,
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Function to return the inverse of the matrix stored in a makeCacheMatrix (mCM) object
## or calculate, store, and return a new inverse if no value is stored
cacheSolve <- function(mx, ...) {
	## Retrieve the inverse stored in the mCM object
	cachedInverse <- mx$getInverse()
	
	## If a value is stored, return it
	if(!is.null(cachedInverse)) {
		message("getting cached data")
		return(cachedInverse)
	}
	
	## If no value is stored:
	##   1) Calucate the inverse of the stored matrix
	##   2) Store the new value in the mCM object
	##   3) Return the new value
	newMatrix <- mx$getMatrix()
	newInverse <- solve(newMatrix, ...)
	mx$setInverse(newInverse)
	newInverse
} 