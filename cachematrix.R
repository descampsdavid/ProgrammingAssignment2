## Put comments here that give an overall description of what your
## functions do

##creates a special 'matrix' with a setter 
##and a getter for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, #setter of the matrix
       get = get, #getter of the matrix
       setinverse = setinverse, #setter of the inversed
       getinverse = getinverse) #getter of the inversed
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## if the inversed matrix has already been cached
  ## just return it
  if(!is.null(m)) {
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
