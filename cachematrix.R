# A pair of functions that cache the inverse of a matrix
# and creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
# Initializing the inverse property
i <- NULL

# Function to set the matrix
set <- function( matrix ) {
m <<- matrix
i <<- NULL
}

# Fumction to get the matrix
get <- function() {
m
}

# Function to set the inverse of the matrix
setInverse <- function(inverse) {
i <<- inverse
}

# Function to get the inverse of the matrix
getInverse <- function() {
i
}

# To return the list of methods
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}

# Function to compute the inverse of the special matrix returned by "makeCacheMatrix"
# If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
m <- x$getInverse()
# Just return the inverse if its already set
if( !is.null(m) ) {
message("Already inversed,getting cached data")
return(m)
}
# Get the matrix from our object
data <- x$get()
# Calculate the inverse using matrix multiplication
m <- solve(data) %*% data
# Set the inverse to the object
x$setInverse(m)
# Return the matrix
m
}
