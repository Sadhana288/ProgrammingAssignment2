## There is a function called makecacheMatrix 
## makechacheMatrix consists of set, get, setInverse and gerInverse


makecacheMatrix <- function(x = matrix()){
  inv <- NULL              ##initialising inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}     ##function to get the matrix
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This is used to get the cache data
cacheSolve <- function(x, ...){ ##gets the cache data
  inv <- x$getInverse()
  if(!is.null(inv)){            ##checking whether inverse is null
    message("Getting cached data")
    return(inv)                 ## return inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv                         ##return a matrix that is inverse of x
}


