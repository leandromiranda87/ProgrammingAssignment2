# Create a "matrix"
# and cache its inverse

makeCacheMatrix <- function ( x = matrix() ) {
    
    # Cache the inverse of the matrix
    # When the matrix dont exists, we set "NULL".
    inverse <- NULL;
    
    # Set function,
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    
    # Get function
    get <- function() {
        x;
    }
    
    # Set the inverse of the matrix.
    setinverse <- function(inv) {
        inverse <<- inv;
    }
    
    # Get the inverse of the matrix.
    getinverse <- function() {
        inverse;
    }
    
    # Get a instance with its methods.
    list(    set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
    )
}

# Computes the inverse of the "matrix"
# returned by makeCacheMatrix
# If the inverse has already been calculated and the matrix has not changed,
# then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Receive the parameter
    # And check the value of inverse.
    inverse <- x$getinverse();
    
    # if inverse is not null, then get the cached data
    if( !is.null(inverse) ) {
        message("getting cached data");
    }
    # Else, calculate the value
    else {
        data <- x$get();
        inverse <- x$setinverse(solve(data));
    }
    inverse;
    
    ## Return a matrix
}