#' @title Initial Beta
#' @description Calculate Initial Beta
#'
#' @param X a matrix of beta
#' @param y a numerical vector
#'
#' @return Initial Beta
#' @export
initialbeta<-function(X,y){
  betai=solve(t(X)%*%X)%*%t(X)%*%y
  return(betai)
}
