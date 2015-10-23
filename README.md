# R-programming-Assignment-2
R programming Assignment 2
## makeCacheMatrix is a funtion which creates a
## speical matrix object that can cache its inverse.
## cacheSolve is a function which computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated( and the matrix has not changed),
## then the cashesolve should retrive the inverse from the cache.
## First Function:
## set the value of the matrix
## get the value of the matirx
## set the value of inverse of the matrix
## get the value of inverse of the matrix
makeMatrix <- function(x = matirx()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##The second function calculates the mean of the special "vector" created
##with the above function. 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

##Test
## >x = rbind(c(1, 2), c(2, 1))
## >y = makeMatrix(x)
## >y$get()
##[,1] [,2]
##[1,]    1    2
##[2,]    2    1
##> cacheSolve(y)
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> cacheSolve(y)
##getting cached data
##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
