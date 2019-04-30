## These functions create a special matrix and cache its inverse, which is computationally
#more efficient than computing the inverse every time it is needed.

## The first function creates a matrix and it has a list of functions able to set the value of the matrix (set),
#retrieve its value (get), compute its inverse (setinv) and give it back (getinv).

makeCacheMatrix <- function(x = matrix ()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function () inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The second function retrieves the inverse stored in cache. If that is possible, it stops. Otherwise, it uses the get 
#function defined previously to obtain the matrix we defined and it computes its inverse, caching it via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
