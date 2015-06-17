## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set = function(y) {
            # The job of an environment is to associate,
            # or bind, a set of names to a set of values.
            # The deep assignment arrow, <<-, assigns a value to an object
            # in an environment that is different from the current environment. 
            x <<- y
            inv <<- NULL
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## The cacheSolve function returns inverse to makeCacheMatrix()

cacheSolve <- function(x, ...) {
      inv = x$getinv()
      
      # if the inverse has already been calculated
      if (!is.null(inv)){
            # get it from the cache and skips the computation. 
            message("Pulling cached data")
            return(inv)
      }
      
      # otherwise, calculates the inverse 
      mat.data = x$get()
      inv = solve(mat.data, ...)
      
      # sets the value of the inverse in the cache via the setinv function.
      x$setinv(inv)
      
      return(inv) 
}
