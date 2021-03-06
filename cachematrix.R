
## two functions that are used to create a special object that
# # stores an array and caches its inverse
# # Esta primera función crea un objeto "matriz" especial que puede almacenar en caché su inverso.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL      
    set <- function(y){
          x <<- y
          inv <<- NULL
}
get <-  function () {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# # cacheSolve is a function that computes the inverse of the special "array"
# # returned by makeCacheMatrix above. If the inverse has already been calculated
# # (and the array has not changed), then the cachesolve should retrieve the
# # reverse cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
       }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv
}
------- checking ----------
> pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
> pmatrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(pmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> pmatrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
