#' @name   CRV.computeP
#' @rdname CRV.computeP
#'
#' @title  CRV.computeP function & utils

#' @name %<%
#' @rdname CRV.computeP
#' @param  X = X
#' @param  x = x
#' @return \code{(X %<% 1)} returns c(X@lo, 1)
#' @export
"%<%" <- function(X, x) {}
setMethod("%<%", "ContinuousRV", function(X, x){
  if(class(X) == "ContinuousRV") {
    return(c(X@lo, min(x, X@hi)))
  }
})

#' @name %>%
#' @rdname CRV.computeP
#' @export
"%>%" <- function(X, x) {
  if(class(X) == "ContinuousRV") {
    return(c(max(x, X@lo), X@hi))
  }
}

#' @name %>=%
#' @rdname CRV.computeP
#' @export
"%>=%" <- function(X, x) {
  if(class(X) == "ContinuousRV") {
    return(c(max(x, X@lo), x@hi))
  }
}

#' @name %<=%
#' @rdname CRV.computeP
#' @export
"%<=%" <- function(X, x) {
  if(class(X) == "ContinuousRV") {
    return(c(X@lo, min(x, X@hi)))
  }
}

"%OR%" <- function(X, Y) {
  #pass
}

"%AND%" <- function(X, Y) {
  #pass
}

## CRV.computeP -: calculeaza probabilitatea unor evenimente
#' @name   CRV.computeP
#' @rdname CRV.computeP
#' @param  CRV
#' @param  c(_lower_bound, _upper_bound)
CRV.computeP <- function(obj, x, y = NULL) {}
setMethod("CRV.computeP", "ContinuousRV", function(obj, x) {
  if(class(obj) != "ContinuousRV") {
    stop("unfitting object.class")
    return()
  }

  lower.bound <- x[1]
  upper.bound <- x[2]
  if(lower.bound < X@lo | upper.bound > X@hi) {
    stop("(_lower.bound, _upper.bound) trebuie sa fie inclus in (X@lo, X@hi)")
    return()
  }
  result <- integrate(obj@fun, lower = lower.bound, upper = upper.bound)$value

  return(result)
})
