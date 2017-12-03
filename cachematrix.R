## This function creates an R object that stores a matrix and its inverse

## Initialize function name and set default value (empty matrix) to a formal argument x
makeCacheMatrix <- function(x = matrix()) {
      
      ## Assign free argument s (result of the inverse) to NULL
      s <- NULL
      
      ## Function "set" assignes value of free variable y to the value of x (a matrix)
      ## that is an object in parent environment of function "makeCacheMatrix",
      ## resets value of s in parent environment of function "makeCacheMatrix" to NULL
      ## that clears any previous calculation of the inverse matrix, if there was any
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      
      ## Function "get" is assigned with the value of the matrix
      get <- function() x
      
      ## Function "setinv" assigns to the objects s of parent environment the input argument
      setinv <- function(solve) s <<- solve
      
      ## Function "getinv" is assigned with the value of s
      getinv <- function() s
      
      ## Create output vecor of 4 functions 
      list(set = set,
           get = get,                       
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above
## or pulls an inverse matrix from cache if it is stored there

## Initialize function name that has single formal argument x
cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      s <- x$getinv()
      
      ## Check if the inverse value has been already calculated and stored in cache
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      
      ## If the cached inverse matrix is NULL, then the calculation of inverse commences
      data <- x$get()
      s <- solve(data, ...)
      x$setinv(s)
      s
}
