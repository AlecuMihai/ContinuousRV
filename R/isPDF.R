## isPDF -: determina daca o functie nenula pe un interval _a, _b este
## sau nu densitate de probabilitate
#' @name isPDF
#' @docType package
#' @param function
#' @param lower_bound
#' @param upper_bound
#' @examples
#' # isPDF(function(x) {
#' 16671/41788*exp(1)**(-x**2/2)
#' })

isPDF <- function(fun, fun.lower = -Inf, fun.upper = Inf) {
  out <- tryCatch({
    lower_bound <- fun.lower
    upper_bound <- fun.upper
    if(is.infinite(fun.lower))
      lower_bound <- -(10**3)
    if(is.infinite(fun.upper))
      upper_bound <-  10**3

    data <- seq(lower_bound, upper_bound, length.out = 10000)
    results <- apply(as.data.frame(data), MARGIN = 1, FUN = fun)

    if(length(which(results < 0)) != 0) {
      is.Positive = F
    }
    else {
      is.Positive = T
    }

    if(isTRUE(is.Positive)) {
      result <- integrate(fun, lower = fun.lower, upper = fun.upper)$value

      if(as.numeric(round(result, 3)) == 1) {
        return(T)
      }
      else {
        return(F)
      }
    }
    else {
      return(F)
    }
  },
  warning = function(cond) {
    ##pass
  },
  error = function(cond) {
    message("Error while computing 'isPDF()': ")
    message(cond)
    return()
  })
}
