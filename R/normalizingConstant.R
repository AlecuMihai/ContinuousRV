## normalizingConstant -: determina constanta de normalizare pentru o functie
## nenula pe un interval a, b
#' @name normalizingConstant
#' @docType package
#' @param function
#' @param lower_bound
#' @param upper_bound
#' @examples
#' # Determina constanta de normalizare '(4*x - 2*x**2)' nenula pe intervalul (0, 2)
#' result <- normalizingConstant(function(x) {4*x - 2*x**2}, 0, 2)

normalizingConstant <- function(fun, fun.lower = -Inf, fun.upper = Inf) {
  out <- tryCatch({
    result <- integrate(fun, lower = fun.lower, upper = fun.upper)$value

    if(as.integer(result) == 0) {
      message("Functia nu prezinta o constanta de normalizare!")
      return()
    }
    else {
      message("Constanta de normalizare: ", 1/result)
      return(1/result)
    }
  },
  warning = function(cond) {
    message(cond)
  },
  error = function(cond) {
    message("Functia introdusa nu este valida: ")
    message(cond)
    return()
  })
  return(out)
}
