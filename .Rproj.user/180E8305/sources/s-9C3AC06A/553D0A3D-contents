## CRV.computeMargDen -: determina densitatile marginale a doua v.a. utilizant
## de densitatea comuna a acestora

library(Ryacas)
require(Ryacas)

#' @name   CRV.computeMargDen
#' @rdname CRV.computeMargDen
#' @docType package
#' @param X = CRV
#' @param Y = CRV
#' @param fun = function
#' @returns N/A
#' prints the two corresponding functions
#' @example
#' X <- CRV(fun = function(x) {x}, lo = 0, hi = 1)
#' Y <- CRV(fun = function(x) {x}, lo = 0, hi = 2)
#' com <- function(x, y) {
#'  0.2 * (x + y + 1)
#' }

CRV.computeMargDen <- function(X, Y, fun) {}
setMethod("CRV.computeMargDen", "ContinuousRV", function(X, Y, fun){

  if(class(X) != "ContinuousRV" | class(Y) != "ContinuousRV") {
    stop("Unfitting object.class")
    return()
  }

  x <- ysym('x')
  y <- ysym('y')

  integrand.x <- integrate(fun(x, y), y)
  integrand.y <- integrate(fun(x, y), x)

  f_x <- eval(yac_expr(integrand.x$yacas_cmd), list(y = Y@hi)) -
    eval(yac_expr(integrand.x$yacas_cmd), list(y = Y@lo))

  f_y <- eval(yac_expr(integrand.y$yacas_cmd), list(x = X@hi)) -
    eval(yac_expr(integrand.y$yacas_cmd), list(x = X@lo))

  cat(sprintf("F(X) = \ (%s)\n", f_x))
  cat(sprintf("F(Y) = \ (%s)\n", f_y))

  fun_noWs <- trimws(deparse(com)[3])

  print("Densitati de repartitie conditionate:")
  cat(sprintf("F(X|Y = y) =\ (%s)\\(%s)\n", f_x, fun_noWs))
  cat(sprintf("F(X|Y = y) =\ (%s)\\(%s)", f_y, fun_noWs))
})

