#Initial values for optimization obtained from the least-squares formula (XTX)−1XTy
initialbeta<-function(X,y){
  solve(t(X)%*%X)%*%t(X)%*%y
}

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


estBeta<-function(X,y){
  betahat<-initialbeta(X,y)
  betan<-optim(betahat,optimizing,X,y)
  result<-list(betahat,betan)
  return(result)
}


#Bootstrap Confidence intervals: the user must be able to choose (i) the significance level α to obtain for the 1−α
#confidence intervals for β , and (ii) the number of bootstraps which by default will be 20.
interval<-function(X,y,alpha,B=20){
  n = nrow(X)
  p = ncol(X)
  
  beta_B = matrix(0,p,B)
  
  for (b in 1:B) {
    ind = sample(1:n,n,replace = TRUE)    
    X_b = X[,ind]
    y_b = y[ind]
    
    beta_B[,b] = estBeta(X_b,y_b)
  }
  
  upper = apply(beta_B, 1, quantile, 1-alpha)
  lower = apply(beta_B, 1, quantile, alpha)
  
  beta_ci = cbind(lower,upper)
  
  return(beta_ci)
}

#Plot of the fitted logistic curve to the actual values 

logiplot <- function(X, y, beta) {
  fitmd <- optimizing(X, y, beta)
  predata <- data.frame(X = seq(min(X), max(X), len = 1000))
  
  yhat = predict(fitmd, predata, type = "response")
  
  plot(y ~ X, col = "green")
  lines(y ~ X, yhat, lwd = 2, col = "blue")
}

#The resulting ``Confusion Matrix’’ (see this link) using a cut-off value for prediction at 0.5 (i.e. assign value 1 for predictions above 0.5 and value 0 for prediction below or equal to 0.5). In addition, based on this cut-off value, also output the following metrics:
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

