## `makeCacheMatrix`: This function creates a special "matrix" object
#     that can cache its inverse.
## `cacheSolve`: This function computes the inverse of the special
#     "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then
#     `cacheSolve` should retrieve the inverse from the cache.

# `makeCacheMatrix`: This function creates a special "matrix" object
#     that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL
  set <- function(y){
    x <<- y
    iM <<- NULL
  }
  get <- function() x
  setInv <- function(invMatrix) iM <<- invMatrix
  getInv <- function() iM

  # return the inverted matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# `cacheSolve` computes the inverse of the special
#     "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then
#     `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  iM <- x$getInv()
  if (!is.null(iM)) {
    message("getting caching inverted Matrix")
    return(iM)
  }
  Mat <- x$get()
  iM <- inverse(Mat,...)
  x$setInv(iM)
  ## Return a matrix that is the inverse of 'x'
  iM
}
