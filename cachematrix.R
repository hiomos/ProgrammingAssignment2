## Programming Assignment 2: Lexical Scoping 
## Coded by: Heverth Osorio Molina
## 26 July 2014

## This function creates a special "vector", which is really a list containing a function 
## to set and get a matrix, and to set and get the inverse of the same matrix.
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL # Initializing the object "minv" where the inverse of the matrix will be store.
        set <- function (y) { 
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        invmatrix <- function(solve) minv <<- solve
        getmatrix <- function() minv
        list(set = set, get = get,
             invmatrix = invmatrix,
             getmatrix = getmatrix)
}

## This function check if minv has already been calculated, if not then solve the matrix 
## and store the data
cachesolve <- function(x, ...) {
        minv <- x$getmatrix()
        
        if(!is.null(minv))	{ ## check if minv has already been calculated, if true, return(minv)
                message("getting cached data")
                return(minv)
        }
        ## if minv is not calculated, solve the matrix and store the data.
        mdata <- x$get()
        minv <- solve(mdata, ...)
        x$invmatrix(minv)
        minv
}


