#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse matrix.

##The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##And return a list containing a function to:
#Set the value of matrix
#Get the value of matrix
#Cache the value of inverse matrix
##Get the value of inverse matrix

##The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

#Example
#Create a matrix 2x2:
#matrix=rbind(c(1, -1/2), c(-1/2, 1))
#Call the function cacheSolve: 
#cacheSolve(makeCacheMatrix(x=matrix))


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #Initialize inverse matrix value
    inverse <- NULL
    
    #Set the value of matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #Get the value of matrix
    get <- function() x
    
    #cache the value of inverse matrix
    setinverse <- function(matrixinverse) inverse <<- matrixinverse
    
    #Get the value of inverse matrix
    getinverse <- function() inverse
    
    #Return a list with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function computes the inverse matrix(solve) of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    #Get the value of inverse matrix
    inverse <- x$getinverse()
    
    #If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #Else, get the value of matrix
    data <- x$get()
    
    #calculate the inverse of the matrix
    inverse <- solve(data, ...)
    
    #Set the cache value of inverse matrix
    x$setinverse(inverse)
    
    #Return a matrix that is the inverse of 'x'
    inverse
}
