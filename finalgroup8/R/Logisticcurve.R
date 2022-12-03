#' Logistic curve
#' @description This function is used for Ploting the logistic curve
#' @param X a matrix of numerical coefficient of beta
#' @param y a numerical vector
#' @param beta the coeffcient vector
#'
#' @export
#'
logiplot <- function(X, y, beta) {
  fitmd <- optimizing(X, y, beta)
  predata <- data.frame(X = seq(min(X), max(X), len = 1000))

  yhat = predict(fitmd, predata, type = "response")

  plot(y ~ X, col = "green", main = "Logistic Curve", xlab = "X", ylab = "p")
  lines(y ~ X, yhat, lwd = 2, col = "blue")
}
