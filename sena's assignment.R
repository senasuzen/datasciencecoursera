#1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()){
  c <- NULL
  set <- function(b){
    a <<- b
    c <- NULL
  }
  get <- function() a
  setiverse <- function(inverse) c <<- inverse
  getinverse <- function() c
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(a, ...){
  r <- a$getinverse()
  if(!is.null(r)){
        message("getting cached data")
        return(r)
  }
  mydata <- a$get()
  c <- solve(mydata, ...)
  x$setinverse(c)
  c
}
