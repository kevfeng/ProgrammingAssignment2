## Creates a matrix object and solves the matrix, unless the inverse
## has already been solved upon which the cached inverse is returned

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ## Input x is a matrix
    
    i <- NULL                  ## Set i to NULL at start of function
    
    set <- function(y) {       ## Set global variables
        x <<- y
        i <<- NULL
    }
    get <- function() x        ## Function to return the original value of x
    
    setinverse <- function(solve) i <<- solve ## Function to return the inverse
    ## of the provided matrix and set
    ## to global variable i
    
    getinverse <- function() i                ## Return already cached value
    
    list(set = set, get = get,                ## List of methods to call
         setinverse = setinverse, 
         getinverse = getinverse)
}

## Computes the inverse of the "matrix" returned by the function makeCacheMatrix
## If the inverse has already been calculated, retrieve inverse from cache

cacheSolve <- function(x, ...) {              ## x is the object defined
    ## by makeCacheMatrix()
    
    i <- x$getinverse()             ## Accesses the value of the
    ## inverse in the object
    
    if(!is.null(i)) {                   ## If i is not NULL, print msg and get value
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()             ## If i is NULL, get the original matrix   
    i <- solve(data, ...)       ## Calculate the mean    
    x$setinverse(i)             ## Store the calculated mean  
    i                           ## Return the mean
}