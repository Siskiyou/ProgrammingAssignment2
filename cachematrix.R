##When a user calls the makeCacheMatrix function, they can enter a square matrix (equal column and row lengths)
##The inverse of that matrix will then be assigned to an output variable and stored in cache
## The cacheSolve function allows a user to assign a matrix variable to it and will then return the inverse of that matrix
## If no matrix variable is assigned by a user, the inverse matrix stored in the cache will be returned

##makeCacheMatrix receives a previously defined matrix and stores the inverse matrix in the cache
makeCacheMatrix <- function (inputMatrix = matrix()) {
  ##Define setter and initialize input and output values
  outputMatrix <- NULL
  set <- function(cacheMatrix) {
    inputMatrix <<- cacheMatrix 
    outputMatrix <<- NULL
  }
  ##Define getter for input and getter/setter for inverse matrices
  get <- function() inputMatrix
  setinverse <- function(solve) outputMatrix <<- solve
  getinverse <- function() outputMatrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(matrixEntry, ...)
{
  ##If no new matrix is assigned to cacheSolve, then the cached inverse matrix will be assigned
  cacheMatrix <- matrixEntry$getinverse()
  if(!is.null(cacheMatrix))
  {
    message("Getting Cached Matrix")
    return(cacheMatrix)
  }
  data <- matrixEntry$get()
  cacheMatrix <- solve(data, ...)
  matrixEntry$setinverse(cacheMatrix)
  cacheMatrix
}
