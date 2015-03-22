#The two functions makeCacheMatrix() and cacheSolve() work in tandem to take a 
#matrix, calculate its inverse, and cache that inverse matrix as an object.

#The first function creates a list of several new functions which
#facilitate caching a solved inverse matrix.  This list can then be passed
#to cacheSolve to calculate the inverse matrix and cache it.

makeCacheMatrix <- function(x = matrix()){
    inverse_x <- NULL
    get <- function() x
    set_inv <- function(inv) inverse_x <<- inv
    get_inv <- function() inverse_x
    reset_matrix <- function(y){
        x <<- y
        inverse_x <<- NULL
    }
    list(get = get, 
         set_inv = set_inv,
         get_inv = get_inv,
         reset_matrix = reset_matrix)
}


#The first time the list created in makeCacheMatrix is passed to cacheSolve, 
#it calculates the inverse of the matrix originally passed into 
#makeCacheMatrix and caches the inverse matrix. Subsequent calls 
#of the same list return the cached inverse matrix.

cacheSolve <- function(x_list, ...){
    inverse_x <- x_list$get_inv()
    if(!is.null(inverse_x)){
        message("getting cached data")
        return(inverse_x)
    }
    data <- x_list$get()
    inverse_x <- solve(data, ...)
    x_list$set_inv(inverse_x)
    inverse_x
}
