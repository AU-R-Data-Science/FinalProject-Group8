#' @title Optimizing function
#' @description Based on the equation to get beta
#' @param beta the coefficient vector
#' @param X a matrix of beta
#' @param y a numerical vector
#' @return Sum of beta hat
#' @export
optimizing<-function(X,y,beta){
  prob<-c()
  b<-c()
  n=length(y)
  for (i in 1:n){
    prob[i]<-1/1+exp(t(-X[i,])%*%beta)
  }

  for (i in 1:n){
    b[i]<-(-y[i]*log(prob[i])-(1-y[i])*log(1-prob[i]))
  }
  return(sum(b))
}
