## creates an 'object' with the following functions/'methods':
##  $set
##  $get
##  $setMatrix
##  $getMatrix
makeCacheMatrix <- function(aMatrix = matrix()) {
  # initialize cache to 'empty'
  matrixCache <- NULL
  
  # setter function for the aMatrix 'member'
  # also NULL's out the existing matrixCache
  set <- function(y) {
    aMatrix <<- y
    matrixCache <<- NULL
  }
  
  # getter function, just returns the aMatrix 'member' 
  get <- function() aMatrix
  
  # setter function, sets the matrixCache
  setMatrix <- function(toSolve) matrixCache <<- toSolve
  
  # getter function, gets the matrixCache
  getMatrix <- function() matrixCache
  
  # wrapper around the 'inner functions' so that
  # we can access by using the '$' shortuct varName$.set
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Return a matrix that is the inverse of 'x'
## 
cacheSolve <- function(x=matrix(), ...) {
  
  m <- x$getMatrix()
  
  # return the cached value if found, otherwise solve
  # and add that value to the cache for future lookup
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
