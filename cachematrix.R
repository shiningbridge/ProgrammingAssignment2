## `makeCacheMatrix`: This function creates a special "matrix" object
#     that can cache its inverse.
## `cacheSolve`: This function computes the inverse of the special
#     "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then
#     `cacheSolve` should retrieve the inverse from the cache.

# `makeCacheMatrix`: This function creates a special "matrix" object
#     that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL # iM is the value to be cached
  set <- function(y){
    # set the special matrix value into cache
    x <<- y
    # in cache remove the old cached value
    # by set the value to NULL, since the original
    # matrix has been changed.
    iM <<- NULL
  }
  get <- function() x
  # pass the inverted matrix 'invMatrix' to cache
  setInv <- function(invMatrix) iM <<- invMatrix
  # get inverted Matrix
  getInv <- function() iM

  # return the functions list which can be seen as
  # a special Matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# `cacheSolve` computes the inverse of the special
#     "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then
#     `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # x is computed by 'makeCacheMatrix' function
  # iM in cache is either 'NULL'
  # or cached value that invert the special matrix
  iM <- x$getInv()
  # if iM is NOT 'NULL'
  if (!is.null(iM)) {
    message("getting caching inverted Matrix")
    # return explicitly the cached value. Fast and easy!
    return(iM)
  }
  # Otherwise the iM has not been calculated before
  # or has been set to 'NULL' by set() function defined in makeCacheMatrix()
  Mat <- x$get()
  # Real calculation should happen.
  iM <- solve(Mat)
  # Now put the iM into cache
  x$setInv(iM)
  ## Return a matrix that is the inverse of 'x'
  # this 'x' is the special matrix that returned from
  # makeCacheMatrix() function, in fact, a list.
  iM
}
