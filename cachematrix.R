## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Invcache <- NULL ## initialize inverse
  
  ## set x. If inverse is already set, get rid of it!
  set <- function(y = matrix()) {
    x <<- y 
    Invcache <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable to desired value
  setInverse <- function(invVal) Invcache <<- invVal 
  
  getInverse  <- function() Invcache
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=makeMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided a test 2x2 matrix
  
  ## If there's something there already
  m <- x$getInverse() 
  ## If there's a cached value AND it's a matrix
  if(!is.null(m) && is.matrix(m)) { 
    message("Cached data is found")
    return(m)
  }
  
  ## otherwise get the matrix
  data <- x$get()  
  ## solve the matrix
  m <- solve(data)
  ## set the value of the inverse
  message("Setting the value of inverse to:") 
  x$setInverse(m)
}
