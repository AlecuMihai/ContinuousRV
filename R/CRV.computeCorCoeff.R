## CRV.computeCorCoeff -: determina coeficientul de corelatie dintre doua v.a.
## continue - folosing densitatea comuna a celor doua variabile
#' @name CRV.computeCorCoeff
#' @rdname CRV.ComputeCorCoeff
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
