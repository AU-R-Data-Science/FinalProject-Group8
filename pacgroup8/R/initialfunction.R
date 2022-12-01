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
