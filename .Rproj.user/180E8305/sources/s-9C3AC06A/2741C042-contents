## Continuous Random Variable
## slots = c(__argument_list)
## fun -> densitatea de repartitie asociata v.a.
##        se considera ca functia respecta proprietatile unei
##        _PDF -: probability density function :
##        1. este pozitiva, pentru orice x apartinand multimii nr. reale
##        2. integrate(fun, lower = -Inf, upper = Inf) = 1
##        3. _
##        Functia se considera nula pe intervalele (-Inf, a), respectiv (b, Inf)
##        cu a = lo, b = hi.
##        In cazul in care aceste valori nu sunt completate, lo, hi primesc
##        automat valorile -Inf, respectiv Inf.

#' @name CRV
#' @docType package
#' @param function
#' @param lower_bound
#' @param upper_bound
#' @example
#' c <- CRV(fun = function(x) {x + 1}, lo = 0, hi =  10)

CRV <- setClass("ContinuousRV",
                slots = c(fun = "function", lo = "numeric", hi = "numeric"),
                prototype = list(lo = -Inf, hi = Inf))

##        cazul bidimensional -: se adauga o noua functie, nenula pe intervalul
##        sec.lo, sec.hi
#' @name BiCRV
#' @param CRV
#' @param function
#' @param second_lower_bound
#' @param second_upper_bound
#' @example
#' c <- BiCRV(CRV(fun = function(X) { x + 1 }, lo = 0, hi = 10),
#'                sec.fun = function(y) { y + 7 }, sec.lo = 10, sec.hi =20)

BiCRV <- setClass("BidimensionalCRV",
                  slots = c(sec.fun = "function", sec.lo = "numeric", sec.hi = "numeric"),
                  contains = "ContinuousRV",
                  prototype = list(sec.lo = -Inf, sec.hi = Inf))

setMethod("print", "ContinuousRV", function(x) {
  print(x@fun)
  cat(paste("lower_limit =", x@lo,
            "upper_limit =", x@hi))
})

setMethod("print", "BidimensionalCRV", function(x) {
          print(x@fun)
          cat(paste("lower_limit_fun =", x@lo,
                    "upper_limit_fun =", x@hi))
          print(x@sec.fun)
          cat(paste("lower_limit_fun =", x@lo,
                    "upper_limit_fun =", x@hi))
  })
