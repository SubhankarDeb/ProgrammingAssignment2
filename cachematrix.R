## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function





#library(MASS) is used to calculate inverse for non squared as well as square matrices



library(MASS)

makeCacheMatrix <- function(x= matrix()) {
  inv<- NULL
  
  set<-function(y) {    #initializing inverse as NULL
                     x<<-y
                     inv<<-NULL
                   }
  get<-function()x  #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                     inver<-ginv(x)
                     inver%*%x
                    }
      list (set= set, get=get,
            setin= setinv,
            
            getinv=getinv)
}
     

cacheSolve <- function(x, ...) 
  {
    inv<-x$getinv()
    if(!is.null(inv)){
      message("getting cached data!")
      return(inv) 
      }
    #checking whether inverse is Null
    #returns inverse value
    data<-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv ## Return a matrix that is the inverse of 'x'
    
}
