## The first function makeCacheMatrix creates a function to inverse the matrix and hold it in cache and the second
## function CacheSolve will return the inverse of the matrix 

## makeCacheMatrix creates a matrix that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
        x <<- y
        m_inverse <<- NULL
        }
            get <- function() x
            setInverse <- function(solve) m_inverse <<- solve
            getInverse <- function() m_inverse
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## cacheSolve gets the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getInverse()
        if(!is.null(m_inverse)) {
          message("getting cached data")
          return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data)
        x$setInverse(m_inverse)
        m_inverse      
}
