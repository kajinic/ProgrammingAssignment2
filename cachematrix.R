## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.
## makeCacheMatrix function.
##
## Usage example:
## x <- matrix(1:9, nrow=3, ncol=3)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
    
    # Initially set it to NULL

    inv_matrix <- NULL
    
    # Set a function
    # It sets the matrix itself but not its inverse
    
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    
    # Get the function
    # It gets the matrix itself but not its inverse
    
    get <- function() x
    
    # Set the inverse manually
    
    setinverse <- function(inverse) inv_matrix <<- inverse
    
    # Get the inverse
    
    getinverse <- function() inv_matrix
    
    # Encapsulate into a list
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix function.
##
## If the user tries to use cacheSolve again on the same special
## matrix, then the pre-computed result is returned.
##

cacheSolve <- function(x, ...) {
    
    # Get the current state of the inverse and check if it
    # has already been computed
    
    inv_matrix <- x$getinverse()
    
    if(!is.null(inv_matrix)) {
        
        # Return the computed inverse
        
        message("Getting cached matrix")
        return(inv_matrix)
    }
    
    # Otherwise get the matrix itself
    
    data <- x$get()
    
    # Calculate the inverse
    
    inv_matrix <- solve(data, ...)
    
    # Cache this result in the object
    
    x$setinverse(inv_matrix)
    
    # Return theresult
    
    inv_matrix    
}

## Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## solution_1 <- cacheSolve(m)
## print(solution_1)
##
## solution_1 should return:
##     
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##
## solution_2 <- cacheSolve(m)
## This should display a "Getting cached matrix" message
## print(solution_2)

## solution_2 should return
##     
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
