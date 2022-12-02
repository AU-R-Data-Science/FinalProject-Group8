#' logistic curve
#' Plot of the fitted logistic curve
#' @param X the values of the predictors
#' @param y the values of the response
#' @param beta the loss value beta by logistic function
#'
#' @return curve
#' @export
#' @author Chin-Hung Huang
#' @examples
logiplot <- function(X, y, beta) {
  fitmd <- optimizing(X, y, beta)
  predata <- data.frame(X = seq(min(X), max(X), len = 1000))
  
  yhat = predict(fitmd, predata, type = "response")
  
  plot(y ~ X, col = "green")
  lines(y ~ X, yhat, lwd = 2, col = "blue")
}
