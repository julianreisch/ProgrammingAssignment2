##The functions will first read an invertible matrix and give out a list of information
##if the inverse has been calculated already. Then, it will check whether the inverse is known or not
##already and print it or calculate and print it respectively

##This function creates - given an invertible matrix x -
##a list containing functions set, get, setinverse and getinverse

makeCacheMatrix <- function(x=matrix()){
  
  #set the inverse of the matrix as NULL
  inverse<-NULL
  
  #set the value of the matrix
  set <-function(y){
    x<<-y
    m<<-NULL
  }
  
  #get the value of the matrix
  get<- function() x
  #set the inverse of the matrix
  setinverse<-function(matrixinverse) inverse <<-matrixinverse
  
  #get the inverse of the matrix
  getinverse<- function() inverse
  
  #print our the list of the defined functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  
}


##This function reads a list that as been created by the makeCacheMatrix function on a matrix x
##and checks whether the inverse of the matrix x has been calculated already
##if so, it gives the result, else it calculates the inverse 

cacheSolve<-function(x,...){  
  #Check whether the inverse of x has been calculated already
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    #if so, return the value of the inverse of x
    message("getting cached inverse")
    return(inverse)
  }
  
  #if not, get the value of x...
  data<-x$get()
  #...and calculate its inverse
  inverse<-solve(data,...)
  #Note that there will be an error of x is not invertible!
  
  #Cache the inverse of x so that in future, no more computation is needed :-)
  x$setinverse(inverse)
  
  #give the value of the inverse of x
  inverse
  
}