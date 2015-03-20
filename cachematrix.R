## a pair of functions (makeCacheMatrix, cacheSolve) that cache the inverse of a matrix.

## test data:
## see here: http://www.mathsisfun.com/algebra/matrix-inverse.html for matrix inverse defintion.
    # x <-matrix(c(4,2,7,6), 2,2)
    # l <-makeCacheMatrix(x)

    ## 1st time runs, it returns below, just like solve(x) returns:
    # cacheSolve(l)
    # [,1] [,2]
        # [1,]  0.6 -0.7
        # [2,] -0.2  0.4

    ## 2nd time runs, it gets data from cache:
    # cacheSolve(l)
        # getting cached data
        # [,1] [,2]
        # [1,]  0.6 -0.7
        # [2,] -0.2  0.4

makeCacheMatrix <- function(x = matrix()) {
        ## create a special "matrix", which is a list containing a function to:
        # set the value of matrix,
        # get the value of the matrix
        # set the value of inverse
        # get the value of inverse
  
        i <- NULL
        
        # No.1, set the value of matrix
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        
        # No.2, get the value of the matrix
        get <- function() x
        
        # No.3 set the value of inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        # No.4, get the value of inverse of the matrix
        getinverse <- function() i
        
        # return the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the above makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', 
        ## x is an object (list) similar to the list created in above makeCacheMatrix function.
        
        ## checks to see if the inverse has already been calculated. 
        ## If so, it gets the inverse from the CACHE and skips the computation.
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        ## otherwise, it calculates the inverse of the data
        ## and sets the value of the inverse in the CACHE via the setinverse function.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
