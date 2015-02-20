## The makeCacheMatrix function creates a special "cacheable matrix"
# which is actually a list containing four functions:
# set: sets the value of the matrix
# get: gets the value of the matrix
# setinverse: sets the inverse of the matrix
# getinverse: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # set the internal variable for the inverse (i) to null
        i <- NULL
        
        # define the SET function to set the matrix variable (x)
        # and initialise inverse variable (i) to null
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        
        # define GET function to return the value of the matrix (x)
        get <- function() x
        
        # define SETINVERSE function to set the inverse value (i) 
        setinverse <- function(inverse) i <<- inverse
        
        # define GETINVERSE function to return the inverse value (i)
        getinverse <- function() i
        
        # return a list of the four funtions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function returns the inverse of a cacheable matrix passed to it.
# If the inverse of the matrix is already cached, it returns the cached version
# otherwise it calculates it and stored the result in the cache.

cacheSolve <- function(x, ...) {
        # Get the inverse (i) of the cacheable matrix objet (x)
        i <- x$getinverse()
        
        # If the returned value is not null, then it's because it's a
        # cached result, so just return that. 
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        
        # i is null, so there is no cached version stored.
        
        # Get the matrix data from (x)
        data <- x$get()
        
        # Calculate the inverse of the matrix using solve()
        i <- solve(data, ...)
        
        # Store the result of inverse operation (i) in the cache
        # of the cacheable matrix (x)
        x$setinverse(i)
        
        # Finally, return the inverse (i)
        i
}
