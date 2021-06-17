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
