### creates a "special" matrix  object which caches its inverse. It includes: 
## a.setting a matrix
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
## b. getting a matrix
  get <- function() x

## c. setting an inverse function
  setinverse <- function(inverse) m <<- inverse
## d. getting the inverse function
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### creates a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
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


### TESTINGS
##creates a matrix A
A=matrix(c(2,4,6,8),2,2)

## creates a special matrix B which caches inverse of matrix A
B=makeCacheMatrix(A)

##computes the inverse of B returned by makeCacheMatrix above
cacheSolve(B)
