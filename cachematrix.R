## This set of functions is designed to compute an inverse of matrix or retrieve it
## from cache if it is stored there already. These functions only work with an 
## assumption that a matrix supplied is invertible.

## The following function gives a list object which consists of functions to supply
## the matrix tp invert, retrieve it and input and retrieve the inverse matrix.

makeCacheMatrix <- function(x = numeric()) {
             sol<-matrix()
             setmatr<-function(y) {
               x<<-y
               sol<<-matrix()
             }
             getmatr<-function() x
             setsol<-function(solved) sol<<-solved
             getsol<-function() sol
             list(setmatr=setmatr, getmatr=getmatr, setsol=setsol, getsol=getsol)
}


## The following function determines whether the inverse matrix will be retrieved from
## cache or computed and computes it, if necessary.

cacheSolve <- function(x, ...) {
        sol<-x$getsol()
        if(!all(is.na(sol))) {
          message("result retrieved from cache")
          return(sol)
          }
        input<-x$getmatr()
        sol<-solve(input)
        x$setsol(sol)
        return(sol)
}
