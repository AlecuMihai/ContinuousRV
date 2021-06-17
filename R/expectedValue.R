## CRV.computeEV -: determina media (Expected Value) unei v.a. continue
#' @name CRV.computeEV
#' @docType package
#' @param CRV
#' @example
#' # X <- CRV(fun = function(x) {(3/8) * (4 * x - 2 * x ** 2)}, lo = 0, hi = 2)
#' # EV <- CRV.computeEV(X)

#' @export
CRV.computeEV <- function(obj) {}
setMethod("CRV.computeEV", "ContinuousRV", function(obj) {
  if(class(obj) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }
  tryCatch({
    f <- obj@fun
    g <- function(x) {x}
    f.g <- function(x) {
      f(x) * g(x)
    }
    result <- integrate(f.g, lower = obj@lo, upper = obj@hi)$value
    return(result)},
    warning = function(cond) {
      message(cond)
    },
    error = function(cond) {
      message(cond)
      return()
    })
})

## CRV.computeMean -: calculul mediei unde X este o v.a. cu o distributie
## cunoscuta, iar g este o functie continua
#' @name CRV.computeMean
#' @docType package
#' @param CRV
#' @param function
#' @example
#' # X <- CRV(fun = function(x) {(3/8) * (4 * x - 2 * x ** 2)}, lo = 0, hi = 2)
#' # mean <- CRV.computeMean(X, function(x) {x + 3})

CRV.computeMean <- function(obj, g) {}
setMethod("CRV.computeMean", "ContinuousRV", function(obj, g) {
  if(class(obj) != "ContinuousRV") {
    stop("unfitting object.class")
    return()
  }
  tryCatch({
    f     <- obj@fun
    fun.g <- g

    f.g <- function(x){
      f(x) * fun.g(x)
    }
    result <- integrate(f.g, lower = obj@lo, upper = obj@hi)$value
    return(result)
  },
  warning = function(cond) {
    message(cond)
  },
  error = function(cond) {
    message(cond)
    return()
  })
})

## CRV.computeVariance -: calculul dispersiei unei v.a. continue
#' @name computeVar
#' @param CRV
#' @example
#' # X <- CRV(fun = function(x) {(3/8) * (4 * x - 2 * x ** 2)}, lo = 0, hi = 2)
#' # var <- CRV.computeVariance(X)

CRV.computeVar <- function(obj) {}
setMethod("CRV.computeVar", "ContinuousRV", function(obj) {
  if(class(obj) != "ContinuousRV") {
    stop("unfitting object.class")
    return()
  }
  tryCatch({
    m <- CRV.computeEV(obj)
    integrand <- function(x) {
      (x - m)**2 * obj@fun(x)
    }

    result <- integrate(integrand, lower = obj@lo, upper = obj@hi)$value
    return(result)
    },
    warning = function(cond) {
      message(cond)
    },
    error = function(cond) {
      message(cond)
      return()
  })
})
