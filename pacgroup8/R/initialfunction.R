#'initial beta
#'Estimate the initial beta value
#' @param X value of the predictors
#' @param y  A factor vector indicating the category of response
#' @return The initial beta value
#' @export
#' @author Tonghui Li
#' @examples
initialbeta<-function(X,y){
  bata<-solve(t(X)%*%X)%*%t(X)%*%y
  return(beta)
}

#' sum of the logistic function
#' calaculate the summary based on the logistic function
#' @param X value of the predictors
#' @param y  A factor vector indicating the category of response
#' @return sumary of beta value
#' @export
#' @author Tonghui Li
#' @examples


optimizing<-function(X,y,beta){
  n = nrow(X)
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







#'Beta estimation
#'Estimate the coefficient vector β which includes the independent variables/predictors plus the intercept
#' @param X value of the predictors
#' @param y  A factor vector indicating the category of response
#' @return The optimal coefficient vector β
#' @export
#' @author Tonghui Li
#' @examples
estBeta<-function(X,y){
  betahat<-initialbeta(X,y)
  betan<-optim(betahat,optimizing,X,y)
  result<-list(betahat,betan)
  return(result)
}
