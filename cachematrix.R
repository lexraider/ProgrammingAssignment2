## Assignment 2
## Cache the inverse of a matrix

## Creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
    # Get matrix dimension
    r <- nrow(x)
    c <- ncol(x)
    
    # Create inverse matrix
    i <- matrix(nrow = r, ncol = c)
  
    set <- function(y) {
        x <<- y
    
        # Get new matrix dimension
        newr <- nrow(y)
        newc <- ncol(y)
        
        # Clear the inverse matrix & initialize with new dimension
        i <<- NULL
        i <<- matrix(nrow = newr, ncol = newc)
    }
  
    get <- function() x
  
    setinverse <- function(inverse) i <<- inverse
  
    getinverse <- function() i
  
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes the inverse of the above matrix.
## If the inverse has already been calculated (matrix remains unchanged),
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.na(i[1,1])) {
        message("getting cached data")
        return(i)
    }
  
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}