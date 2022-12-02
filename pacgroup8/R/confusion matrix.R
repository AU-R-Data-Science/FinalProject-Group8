#' Confusion matrix
#' calculate the confusion matrix by comparing the predicted values with the true values
#' @param X the values of the predictors
#' @param y the values of the response
#' @param bhat the loss value beta by logistic function
#' @param cut The cut-off values for measuring metrics
#'
#' @return confusion matrix
#' @export
#' @author Tonghui Li
#' @examples
confusion_matrix<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1

  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))
  conf_mat = cbind(c(tp,fp),c(fn,tn))

  return(conf_mat)
}
