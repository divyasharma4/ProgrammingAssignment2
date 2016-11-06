## Put comments here that give an overall description of what your
## functions do
## the function makeCacheMatrix cointains 4 methods.  
makeCacheMatrix<-function(x=matrix()){
        ## initialize inverse property
        I<-NULL
        ## set the matrix x
        set<-function(m){
                x<<-m
                I<<-NULL
        }
        ## get the matrix x
        get<-function(){
                x               
        } 
        ## set the inverse matrix of x
        setInverse<-function(i){
                I<<-i
        }
        ## get the inverse matrix of x
        getInverse<-function(){
                I
        }
        ## the list of internal methods
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
## function calculate a inversed matrix
cacheSolve <- function( x, ...){
        ## Return a matrix that is the inverse of 'x'
        ## the inversion matrix IM of the matrix x
        IM <- x$getInverse()
        ## calculation of the inversion matrix IM
        if (is.null(IM)) {
                message('The inverse is being calculated')  
                data <- x$get()
                IM <- solve(data,...)
                x$setInverse(IM)
        } else {
                message('Cached Inverse!!')
        }
        IM
}
