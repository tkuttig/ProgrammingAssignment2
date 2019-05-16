## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix function creates a 'special' matrix objects that allows for saving
# a computed inverse of a matrix outside of the functions local environment, i.e. it
# caches it.
# If the underlying matrix is not changed, the inverse does not have to be recalculated 
# if it has already been calculated with the cacheSolve function.
# Instead, if the inv.matrix object exists in the cache, it is just taken from there.

## Write a short comment describing this function

# The inv.matrix object is initialized as NULL. If the set function is called to 
# change the underlying matrix, the global inv.matrix object is reset to NULL.
# The get function can be used to return the initial input matrix
# set.inv.matrix sets the inv.matrix object.
# get.inv.matrix is used to retrieve the cached inv.matrix object.

makeCacheMatrix <- function(x = matrix()) {
    inv.matrix <- NULL
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }
    get <- function() x
    set.inv.matrix <- function(solve) inv.matrix <<- solve
    get.inv.matrix <- function() inv.matrix
    list(set = set, 
         get = get, 
         set.inv.matrix = set.inv.matrix, 
         get.inv.matrix = get.inv.matrix)
}


## Write a short comment describing this function

# This function first uses the get.inv.matrix function of the matrix object to get the 
# cached value of inv.matrix. If it exists (is not NULL), it returns this matrix and 
# prints a message.
# If there is no cached inv.matrix, it gets the input matrix (using the get function),
# calculates the inverse matrix, sets it in the global environment, and returns it.
# If the function is now called again, the function returns the cached inv.matrix without 
# having to recalculate it.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv.matrix <- x$get.inv.matrix()
    if(!is.null(inv.matrix)) {
        message("getting cached invers matrix")
        return(inv.matrix)
    }
    data <- x$get()
    inv.matrix <- solve(data, ...)
    x$set.inv.matrix(inv.matrix)
    inv.matrix
}