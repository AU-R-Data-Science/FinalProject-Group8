#' Calculate confusion_matrix
#' @description {Calculate confusion_matrixthe following metrics:Prevalence,Accuracy,Sensitivity,Specificity,False Discovery Rate,Diagnostic Odds Ratio}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @return confusion_matrix
#' @author Tonghui Li
#' @export
#'
#'
confusion_matrix<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1

  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))
  conf_mat = cbind(c(tp,fp),c(fn,tn))

  return(conf_mat)
}

Prevalence<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1

  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Prevalence = (fn+tp)/(tp+fp+tn+fn)

  return(Prevalence)
}



Accuracy<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Accuracy = (tn+tp)/(tp+fp+tn+fn)

  return(Accuracy)
}


Sensitivity<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Sensitivity = tp/(tp+fn)

  return(Sensitivity)
}


Specificity<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Specificity= tn/(tn+fp)

  return(Specificity)
}


False_Discovery_Rate<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  False_Discovery_Rate= fp/(tp+fp)

  return(False_Discovery_Rate)
}

Diagnostic_Odds_ration<-function(X,y,bhat,cut=0.5){

  yhat = (1/(1+exp(-X%*%bhat))>0.5)*1
  tp = sum((yhat==1)&(y==1))
  fp = sum((yhat==0)&(y==1))
  tn = sum((yhat==0)&(y==0))
  fn = sum((yhat==1)&(y==0))

  Diagnostic_Odds_ration= (sensitivity /False_Discovery_Rate) / (False_Discovery_Rate/ specificity)


  return(Diagnostic_Odds_ration)
}

