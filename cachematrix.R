## This code is meant to cache the results of finding the 
## inverse of a non-varying matrix

## First part creates a matrix to cache a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL           
        set <- function(y = matrix()) {  ##setcachefunction
                x <<- y
                m <<- NULL
        }
        
        get <- function() x              ##getcachefunction
        setinverse <- function(solve) m <<- solve  
        getinverse <- function() m                 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second part computes the inverse of a matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {         ##if soluiton already cached
                message("getting cached data")
                return(m)         ##skip the rest of function
        }
        data <- x$get()           ##if no solution cached
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
