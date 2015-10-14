##The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##which is really a list containing a function to
#Set the value of Matrix
#Get the value of Matrix
#Return a list with the functions
#Set the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    #Initialize inverse matrix value
    inverse <- NULL
    
    #Set the value of Matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #Get the value of Matrix
    get <- function() x
    
    #Set the value of inverse matrix
    setinverse <- function(matrixinverse) inverse <<- matrixinverse
    
    #Get the value of inverse matrix
    getinverse <- function() inverse
    
    #Return a list with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #Get the value of inverse matrix
    inverse <- x$getinverse()
    
    # If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #Else, get the value of matrix
    data <- x$get()
    
    #calculate the inverse of the matrix
    inverse <- solve(data, ...)
    
    #cache the value of inverse matrix
    x$setinverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
