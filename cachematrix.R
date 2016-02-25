## These functions take a square matrix as input, and either compute (and cache)
## the inverse of this matrix, or, if this has already been done, retrieve the
## cached inverse. In either case, the inverse matrix is returned.

## This function takes a square matrix as input, and outputs a list of functions.
## It serves to cache the matrix inverse computed by cacheSolve.

makeCacheMatrix <- function(X = matrix()) {    #optional input here is a matrix
      invX <- NULL            #locally resets invX
      set <- function(Y) {    #1.allows one to (re)assign a different matrix to X
            X <<- Y           #in mCM; it also
            invX <<- NULL     #resets invX in mCM
      }
      get <- function() X     #2.prints the local value of X
      setinv <- function(inv) invX <<- inv    #3.assigns the input value to invX in mCM
      #this function is not intended to be called directly!
      getinv <- function() invX     #4.prints the local value of invX; returns NULL
      #if inverse has not yet been computed
      list(set = set, get = get,              #creates a list of these functions
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the matrix specified in makeCacheMatrix,
## unless this has already occurred, in which case it retrieves this inverse from
## the cache.

cacheSolve <- function(z, ...) {     #here we input the list created by makeCacheMatrix
      invX <- z$getinv()        #locally assigns invX the value of getinv()
      if(!is.null(invX)) {       #if invX has a value, print this message and the value; stop.
            message("getting cached data")
            return(invX)
      }
      matrix <- z$get()         #otherwise, locally assigns the output of get() to "matrix"
      invX <- solve(matrix, ...)    #locally assigns the inverse of this matrix to invX
      z$setinv(invX)            #assigns the inverse of this matrix to invX
      invX                      #prints invX, the matrix inverse
}