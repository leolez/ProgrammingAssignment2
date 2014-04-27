## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInvMat <- function(inverseMatrix) invmat <<- inverseMatrix
  getInvMat <- function() invmat
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInvMat()
  if(!is.null(invmat)) {
    message("getting cached inverse of matrix")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat)
  x$setInvMat(invmat)
  invmat
}
