## These two functions stores a list of functions and retrieves a
## previously calculated inverse of a matrix if the matrix being 
## inverted was used previously.

## makeCacheMatrix creates a list of functions which can be retrieved
## using a different function later on.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,                  # gives the name 'set' to the set() function defined above
             get = get,                  # gives the name 'get' to the get() function defined above
             setinverse = setinverse,    # gives the name 'setinverse' to the setinverse() function defined above
             getinverse = getinverse)    # gives the name 'getinverse' to the getinverse() function defined above
}


## cacheSolve checks whether the matrix m is identical to the 
## matrix being solved. If it is there is a previously inverted matrix
## that can be retrieved.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

