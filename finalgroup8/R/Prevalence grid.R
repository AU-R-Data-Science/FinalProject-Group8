#' @title plot Prevalence over a grid of cut-off value.
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return table of prevalence
#' @export

prevalencegrid<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Prevalence = (fn+tp)/(tp+fp+tn+fn)
  cut<-seq(0.1,0.9,by=0.1)
  Prevalence<-matrix(NA,9,1)
  for (i in 1:9){
    value<-cut[i]
    Prevalence[i,]<-matrix(X,y,value)
  }
  return(Prevalence)

}
