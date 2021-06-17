## CRV.computeCOV -: determina covarianta pentru doua v.a. continue X, Y
## cu repartitia comuna 'double.fun'
#' @name CRV.computeCov
#' @docType package
#' @param X=CRV
#' @param Y=CRV
#' @param double.fun = function
#' @returns covariance
#' @example
#' X <- CRV(fun = function(x) {x ** 2}, lo = 0, hi = 2)
#' Y <- CRV(fun = function(x) {x ** 3}, lo = 7, hi = 10)
#' com <- function(x, y) {
#' x ** 2 + y ** 3
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

## CRV.computeCorCoeff -: determina coeficientul de corelatie dintre doua v.a.
## continue - folosing densitatea comuna a celor doua variabile
#' @name CRV.computeCorCoeff
#' @rdname CRV.ComputeCov
#' @docType package
#' @param X = CRV
#' @param Y = CRV
#' @param CRV.cov = function
#' @returns Correlation Coefficient
#' @export

CRV.computeCorCoeff <- function(X, Y, CRV.cov) {}
setMethod("CRV.computeCorCoeff", "ContinuousRV", function(X, Y, CRV.cov) {
  if(class(X) != "ContinuousRV" | class(Y) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }
  tryCatch({
    var.X <- CRV.computeVar(X)
    var.Y <- CRV.computeVar(Y)

    if(var.X < 0 | var.Y < 0) {
      stop("Variance can not be less than 0")
      return()
    }

    sgn_X = sqrt(var.X)
    sgn_Y = sqrt(var.Y)

    return(CRV.cov/(sgn_X * sgn_Y))
  },
  warning = function(cond) {
    message(cond)
  },
  error = function(cond) {
    message(cond)
    return()
  })
})
