## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.
## makeCacheMatrix defines a special "matrix" object caching its inverse.
## cacheSolve computes the inverse of the special "matrix", but if its has
## been previously calculated the cached inverse will be returned.

## makeCacheMatrix - This function creates a special "matrix" object  that
## 					 can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# Define the inverse variable, used to cache previous solves
	inverse <- NULL

	# Set a new value for the special "matrix"
	# When new matrix value is set the inverse variable must be set to NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
	}

	# Get the value of the matrix
	get<-funtion() x

	# Set the value of the inverse variable with a new value
	setinverse <- function(new_inverse) inverse<<-new_inverse

	# Get the value of the inverse variable of the matrix
	getinverse <- function() inverse

	# Return the list of functions defined
	list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve - This function computes the inverse of the special "matrix"
##				returned by makeCacheMatrix above. If the inverse has already
##				been calculated (and the matrix has not changed), then the
##				cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # First, get the cached value
        inverse <- x$getinverse()

        # If inverse is not nulll, then the cached value is returned
        if(!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }

        # If the cached value is NULL it must be computed again
        # Get he special "matrix"
        data<-x$get()
        
        # The solve function return the matrix inverse of 'x'
        inverse<-solve(data, ...)

        # Set the value of the inverse variable of the special
        # "matrix" in order to no compute again if it's later
        # requested
        x$setinverse(inverse)

        #Return the inverse
        inverse
}
