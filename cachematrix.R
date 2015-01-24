## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix: 
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setMatrix <- function(Matrix) m <<- Matrix
  getMatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)){
    message("Cached data found!")
    return(m)
  }
  else {
    message("No cached data found!")
    data <- x$get() # obtains matrix from object x
    m <- solve(data) # finds inverse matrix
    x$setMatrix(m) # assigns resulting inverse matrix to object x
    return(m)
  }
}
