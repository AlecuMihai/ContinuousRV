\name{CRV.computeCov}
\alias{CRV.computeCov}
\title{
CRV.computeCov
}
\description{
Determina covarianta pentru doua v.a. continue X, Y cu repartitia comuna 'double.fun'.
}
\usage{
CRV.computeCov(X, Y, double.fun)
}
\arguments{
  \item{X}{
  v.a. continua construita utilizand ContinuousRV::CRV.
  }
  \item{Y}{
  v.a. continua construita utilizand ContinuousRV::CRV.
  }
  \item{double.fun}{
  Functie cu doua argumente - (x, y). - densitatea comuna a celor doua v.a.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Cov(X, Y)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Pentru determinarea covariantei se foloseste urmatoare metoda:
\code{
inner.fun <- function(x, y) {\cr
    x * y * double.fun(x, y)\cr
  }\cr
\cr
  M_X_Y <- integrate(function(y) {\cr
    sapply(y, function(y) {\cr
      integrate(function(x) inner.fun(x, y), X@lo, X@hi)$value\cr
      })\cr
    }, Y@lo, Y@hi)$value\cr
\cr
  M_X <- CRV.computeEV(X)\cr
  M_Y <- CRV.computeEV(Y)\cr
\cr
  out <- M_X_Y - M_X * M_Y\cr
}\cr

unde M_X, M_Y, reprezinta media v.a. X, respectiv Y;
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
X <- CRV(fun = function(x){(2197/5412) * (x**2 + 3*x + 1)}, lo = 0, hi = 12/13)
Y <- CRV(fun = function(x) {20000/7708481 * (2 * x**3 - 2)}, lo = 1, hi = 53/10)
com <- function(x, y) {
x + y
}
CRV.computeCov(X = X, Y = Y, double.fun = com)
}
