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
