#' @title Bootstrap Confidence Interval
#' @param X a numeric dataset
#' @param y the significance value
#' @param n the number of bootstraps (default is 20)
#' @return a matrix containing the bootstrap confidence interval
#' @export
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
