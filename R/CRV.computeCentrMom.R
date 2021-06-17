## CRV.computeCentrMom -: determina momentele centrate pana la ordinul 4 pentru o
## v.a. continua
#' @name   CRV.computeCentrMom
#' @rdname CRV.computeCentrMom
#' @docType package
#' @param CRV
#' @example
#' c3 <- CRV(fun = function(x) {(3/8) * (4 * x - 2 * x**2)}, lo = 0, hi = 2)
#' mInit <- CRV.computeCentrMom(c3)
#' @return \code{c(_mom{0:4})}
#' @export

CRV.computeCentrMom <- function(obj) {}
setMethod("CRV.computeCentrMom", "ContinuousRV", function(obj) {
  if(class(obj) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }
  tryCatch({
    result <- c()
    m <- CRV.computeEV(obj)
    fun <- obj@fun

    for(i in 0 : 4) {
      f <- function(x) {
        (x - m)**i
      }
      integrand <- function(x) {
        f(x) * fun(x)
      }

      out <- integrate(integrand, obj@lo, obj@hi)$value
      result <- append(result, out)
    }
    return(result)
  })
})
