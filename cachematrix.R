## The makeCaheMatrix function's purpose is to cache the inverse of a matrix
## makesCacheMatrix takes one augument and caculates and stores the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inv2) inv <<- inv2
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## cachSolve checks to see if the matrix augument passed is, in fact, cashed and,
## if so, returns "getting cached data" and the cashe of the matrix if it's been previously stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}

## Usage:
## inv1to4 <- matrix(1:4, 2, 2)
## inv1to4b <- makeCacheMatrix(inv1to4)
## cacheSolve(inv1to4b)
## (cashes inverse)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## cacheSolve(inv1to4b)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5