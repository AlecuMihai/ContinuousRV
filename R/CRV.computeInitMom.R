## CRV.computeInitMom -: determina momentele initiale pana la ordinul 4 pentru o
## v.a. continua
#' @name   CRV.computeInitMom
#' @rdname CRV.computeInitMom
#' @docType package
#' @param CRV
#' @example
#' c3 <- CRV(fun = function(x) {(3/8) * (4 * x - 2 * x**2)}, lo = 0, hi = 2)
#' mInit <- CRV.computeInitMom(c3)
#' @return \code{c(_mom{0:4})}
#' @export

CRV.computeInitMom <- function(obj) {}
setMethod("CRV.computeInitMom", "ContinuousRV", function(obj) {
  if(class(obj) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }
  tryCatch({
    result <- c()
    fun <- obj@fun
    for(i in 0 : 4) {
      f <- function(x) {
        x**i
      }
      integrand <- function(x) {
        fun(x) * f(x)
      }
      out <- integrate(integrand, lower = obj@lo, upper = obj@hi)$value
      result <- append(result, out)
    }
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
