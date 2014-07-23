## cacheMatrix.R: Defines a class that contains a matrix and its inverse.
##                Also defines a function to call this class and to generate
##                inverse matrix

## makeCacheMatric: Create a class that contains mx and inverse of mx (if 
##                  previously calculated)
##                  Class includes four methods as a list:
##                    set: stores mx
##                    get: prints mx
##                    setInverse: stores inverse of mx
##                    getInverse: prints inverse of mx

makeCacheMatrix <- function(x = matrix()) {
    # by default, inv is empty
    inv <- NULL
    # set method: y is a new mx, rescope to x; inv is null (new mx)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get method: just print the stored mx
    get <- function() x
    # setInverse method: store the new inv
    setInverse <- function(inverse) inv <<- inverse
    # getInverse method: just print the stored inv
    getInverse <- function() inv
    #return methods as a list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: Create function to use makeCacheMatrix and calculate inverse if 
##             necessary

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    # was inv already calculated? If so, print and exit
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # otherwise, read mx...
    data <- x$get()
    # ...solve inverse of mx...
    inv <- solve(data, ...)
    # ...store inv for reuse...
    x$setInverse(inv)
    # ...print mx.
    inv
}