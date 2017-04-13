## The following functions will return the inverse of a matrix by either retrieving the inverse
## from a cache and returning the cached inverse, or by computing and returning the inverse.


## The makeCacheMatrix creates a special matrix that is able to cache the inverse of a matrix. 
## Specifically, it creates a list that contains a function to:
## 1. Set the value of the matrix (set function)
## 2. Get the value of the matrix (get function)
## 3. Set the value of the inverse of the matrix (setinverse)
## 4. Get the value of the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix. It first checks to see if an inverse 
## has already been calculated, and if so, it will retrieve and return the inverse from the cache directly.
## If the inverse has not been calculated, is computes the inverse via the solve function and 
## returns the result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
