makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get  <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list (set =set, get=get, setInverse = setInverse, getInverse = getInverse )
}

cacheSolve <- function(x,...) {
  inv <- x$getInverse()
  if ( ! is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data  <- x$get()
  inv  <- solve (data,...)
  x$setInverse (inv)
  inv 
}

The following codes are to test the function above. 
a <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
a$get()
a$getInverse()
cacheSolve(a)
