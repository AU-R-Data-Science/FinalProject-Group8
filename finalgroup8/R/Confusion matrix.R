#' @title Confusion matrix
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Confusion matrix
#' @export

confusion_matrix<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))
  conf_mat = cbind(c(tp,fp),c(fn,tn))

  return(conf_mat)
}
