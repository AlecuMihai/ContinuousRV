## CRV.computeCOV -: determina covarianta pentru doua v.a. continue X, Y
## cu repartitia comuna 'double.fun'
#' @name CRV.computeCov
#' @docType package
#' @param X=CRV
#' @param Y=CRV
#' @param double.fun = function
#' @returns covariance
#' @example
#' X <- CRV(fun = function(x){(2197/5412) * (x**2 + 3*x + 1)}, lo = 0, hi = 12/13)
#' Y <- CRV(fun = function(x) {20000/7708481 * (2 * x**3 - 2)}, lo = 1, hi = 53/10)
#' com <- function(x, y) {
#' x + y
#' }
#' CRV.computeCov(X = X, Y = Y, double.fun = com)

#' @export
CRV.computeCov <- function(X, Y, double.fun) {}
setMethod("CRV.computeCov", "ContinuousRV", function(X, Y, double.fun) {
  if(class(X) != "ContinuousRV" | class(Y) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }
  inner.fun <- function(x, y) {
    x * y * double.fun(x, y)
  }

  M_X_Y <- integrate(function(y) {
    sapply(y, function(y) {
      integrate(function(x) inner.fun(x, y), X@lo, X@hi)$value
      })
    }, Y@lo, Y@hi)$value

  M_X <- CRV.computeEV(X)
  M_Y <- CRV.computeEV(Y)

  out <- M_X_Y - M_X * M_Y
  return(out)
})
