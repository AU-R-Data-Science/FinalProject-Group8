#' @title Prevalence
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Prevalence
#' @export
Prevalence<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Prevalence = (fn+tp)/(tp+fp+tn+fn)

  return(Prevalence)
}

#' @title Accuracy
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Accuracy
#' @export
Accuracy<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Accuracy = (tn+tp)/(tp+fp+tn+fn)

  return(Accuracy)
}
#' @title Sensitivity
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Sensitivity
#' @export
Sensitivity<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Sensitivity = tp/(tp+fn)

  return(Sensitivity)
}


#' @title Specificity
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Specificity
#' @export
Specificity<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Specificity= tn/(tn+fp)

  return(Specificity)
}

#' @title False_Discovery_Rate
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return False_Discovery_Rate
#' @export
False_Discovery_Rate<-function(X,y,bhat,cut=0.5){

  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  False_Discovery_Rate= fp/(tp+fp)

  return(False_Discovery_Rate)
}

#' @title Diagnostic_Odds_ration
#' @param x a matrix of numerical coefficients of beta
#' @param y a numerical vector
#' @param bhat the estimated coefficients of the logistic regression
#' @param cut Default is 0.5
#' @return Diagnostic_Odds_ration
#' @export
Diagnostic_Odds_ration<-function(X,y,bhat,cut=0.5){
  yhat = 1/(1+exp(-X%*%bhat))
  ifelse(yhat>0.5,1,0)
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Diagnostic_Odds_ration= (sensitivity /False_Discovery_Rate) / (False_Discovery_Rate/ specificity)


  return(Diagnostic_Odds_ration)
}
