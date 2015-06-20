## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes a matrix and returns a special "matrix" that contains a list of functions to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix created by the above function.
## if the inverse have already been calculated, it will be retrieved and skips computation.
## othewise, it calculate the inverse ofthe matrix and sets the value of the inverse matrix in the
## cache via the setInverseMatrix function
cacheSolve <- function(x, ...) {
    
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMatrix (m)
  ## Return a matrix that is the inverse of 'x'
  m
}

