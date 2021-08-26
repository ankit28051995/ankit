## "makecachematrix" and cachesolve the cache the inverse of a matrix

##makecache is function and comput a special matrix
##cache its inversefor the input

makecachematrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- NULL
    inv <<- NULL
  }
  get <- function() a
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cachesolve is a function which comput inverse  spcl matrix
##returned by makecachematrix above.if inverse already has been calculated
##matrix has not change,
##inverse from the cache

cachesolve <- function(a, ...) {
  #return a matrix that is inverse of 'a'
  inv <- a$getinv()
  if(!is.null(inv)) {
    message("getting cache result")
    return(inv)
  }
  data <- a$get()
  inv  <- solve(data, ...)
  a$setinv(inv)
}

##........checking program..........
##m <- matrix(rnorm(16),4,4)
##m1 <- makecachematrix(m)
## cachesolve(m1)

# [,1]        [,2]        [,3]        [,4]
# [1,]  -0.1653269   0.2592293   0.6176218  -0.7520955
# [2,]   0.2828334  -0.1853499   0.4511382   0.2094365
# [3,]   0.1434840   1.0413868  -0.3550853  -0.3261154
# [4,]   0.1793583  -0.4252171  -0.4371493  -0.1749830