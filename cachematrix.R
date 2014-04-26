# Calculating the inverse of huge matrixes several times can be time consuming.
# This set of functions will be more time efficient when the only problem is recalculation the same matrix.
# Instead of calculating the inverse matrix a second time, it looks the inverse matrix up in the cache.

# makeCacheMatrix() creates a special "matrix" containing a function to 
# set matrix
# get matrix
# calculate the inverse of the matrix using the solve() function
# get the inverse of the matrix


The first function, makeVector creates a special "vector", which is really a list containing a function to 
set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
# 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# cacheSolve() calculates the inverse of the special "matrix" created with makeCacheMatrix(). 
# To do so, it first checks to see if the inverse has already been calculated. 
# If this is the case, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data 
# and sets the inverse of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}



# How to use the functions
# first we need a matrix
# m <- matrix(1:4,2,2)
# second we "transform" the matrix for our inverse function cacheSolved
# m.cached <- makeCacheMatrix(m)
# now we can calculate the inverse
# cacheSolve(m.cached)
# finally we can see, whether the inverse has already been calculated
# cacheSolve(m.cached)
# function will report that it is "getting catched data"
