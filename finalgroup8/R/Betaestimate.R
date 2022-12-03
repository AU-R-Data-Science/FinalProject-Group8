#' @title Beta estimation
#' Calculate estimated beta
#' @param X a matrix of beta
#' @param y a numerical vector
#' @return list of beta and beta n
#' @export
estBeta<-function(X,y){
  bhat<-initialbeta(X,y)
  betan<-optim(bhat,optimizing,X,y)
  result<-list(bhat,betan)
  return(result)
}
