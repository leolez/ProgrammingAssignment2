## 
## The following two functions improve the performance of the 'solve()' when it is used
## for the purpose of calculating the inverse of a matrix, by means of encapsulation and 
## catching
##

## The makeCache Matrix function returns a 'list' including the functions 
## 'set', 'get', 'setInvMat' and 'getInvMat' that encapsulate the interaction
## with the matrix received as parameter and makes the caching process as transparent
## as possible for the user, following OOP patterns.

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


## The 'cacheSolve()' function updates and returns the inverse of the matrix received
## as the 'x' parameter. If there is a previous calculation of the inverse of that
## same matrix, then the cached inverse is returned and no calculation is invoked.
## Please note that the matrix received as the 'x' parameter is expected to be 
## the result of the 'makeCacheMatrix()'

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
