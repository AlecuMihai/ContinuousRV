## CRV.computeMean -: calculul mediei unde X este o v.a. continua cu o distributie
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
