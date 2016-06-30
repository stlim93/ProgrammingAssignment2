## makeCacheMatrix creates a square matrix. If the dimension of
## vector does not match that of a square matrix, the elements are 
## either recycled or ignored. Elements of a matrix can be changed
## by using x$set(y,element) where y is the new value and element is 
## the ith element of the matrix by row (default). Description are 
## given in the function as well.


makeCacheMatrix <- function(x = matrix(),dim,byrow=FALSE) {
        inv<- NULL
        
        # changes certain elements of the vector stored
        # both y and element are vectors of numeric and integer
        set<- function(y,element){
                j<- 1
                for(i in element)
                {x[i] <<- y[j]
                j<j+1}
                inv <<- NULL
        }
        
        get<- function() matrix(x,ncol=dim,nrow=dim,byrow=byrow)
        
        setinverse<- function(inverse) inv <<- inverse
        getinverse<- function() inv
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function attemps to retrieve stored inverse value of the
## special matrix created by makeCacheMatrix. If there is no
## inverse available (i.e. NULL), it computes one and store it in
## the cache.

cacheSolve <- function(x, ...) {
        # Retrieve value of inverse
        # from cache

        inv<- x$getinverse()
        
        # Print result if available from cache otherwise
        # it computes the inverse.
        
        if(!is.null(inv)){
                message("getting cached data")
               return(inv)
        }
        
        # Read data
        data<-x$get()
        
        # Solve for inverse
        inv<- solve(data,...)
        x$setinverse(inv)
        inv
        
}





