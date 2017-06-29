## we are trying to create a function that can be used to cache the inverse of the matrix so that
## we dont need to calculate it again 


## Returns a list of functions upon execution which can be used to set the matrix ,print it ,set the inverse ,print the inverse.  

makeCacheMatrix <- function(m = matrix()) {
  cachedInverse <- NULL
  
  set <- function(y) {
    m <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() {m}
  
  setCachedInverse <- function(inverse){ cachedInverse <<- inverse }
  
  getCachedInverse <- function() {cachedInverse}
  
  list(set = set, get = get,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
}

## Checks if the inverse of the matix is cached then it prints the result or if it is NULL the calculates it and caches it

cacheSolve <- function(l, ...) {
  inverse <- l$getCachedInverse()
  
  if(!is.null(inverse)) {
    print("matrix inverse already cached")
   return(inverse)
   }
  
  
    m <- l$get()
    inverse <- solve(m, ...)
    l$setCachedInverse(inverse)
    
   return(inverse)
}