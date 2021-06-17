\name{CRV.computeCorCoeff}
\alias{CRV.computeCorCoeff}
\title{
CRV.computeCorCoeff
}
\description{
Determina coeficientul de corelatie dintre doua v.a. continue folosind densitatea comuna
a celor doua variabile si covarianta celor doua.
}
\usage{
CRV.computeCorCoeff(X, Y CRV.cov)
}
\arguments{
  \item{X}{
  v.a. construita utilizand ContinuousRV::CRV.
  }
  \item{Y}{
  v.a. construita utilizand ContinuousRV::CRV.
  }
  \item{CRV.cov}{
  Cov(X, Y).
  }
}
\details{
}
\value{
CorCoeff(X, Y) -: coeficientul de corelatie dintre cele doua v.a.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
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
cov <- CRV.computeCov(X = X, Y = Y, double.fun = com)
coeff <- CRV.computeCorCoeff(X, Y, cov)
}

