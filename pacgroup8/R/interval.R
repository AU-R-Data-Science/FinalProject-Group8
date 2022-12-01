#' Calculate confidence intervals by bootstrap
#' @description {Bootstrap Confidence intervals: the user must be able to choose (i) the significance level α  to obtain for the 1−α  confidence intervals for β , and (ii) the number of bootstraps which by default will be 20.}
#' @param X An \eqn{n \times p} \code{double} value of the matrix containing the values of the predictors(Not including intercept).
#' @param Y A factor vector indicating the category of response
#' @param alpha A variable that indicating the significance level
#' @param B A variable vector indicating the number of bootstraps(Default 20)
#' @return Bootstrap Confidence intervals
#' @author Tonghui Li
#' @export
#'
#'
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
