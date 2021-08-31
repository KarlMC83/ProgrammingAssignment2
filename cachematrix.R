## General description: author: Karl Melgarejo. This code sets two functions 
## to cache the inverse of a matrix rather than computing it repeatedly.

## This first function creates a special "matrix" object which is actually
## a list that contains functions, one of them ("setinv") creates a matrix  
## that will cache the inverse of the matrix given.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set =set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## This second function computes the inverse of the special "matrix" returned
## by the previous function (makeCacheMatrix). If the inverse has already been
## calculated then this function shows the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

