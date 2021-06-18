\name{CRV.computeMargDen}
\alias{CRV.computeMargDen}

\title{
CRV.computeMargDen
}
\description{
Determina densitatile marginale si conditionate a doua variable aleatoare
continue (ContinuousRV::CRV) folosindu-se de densitatea comuna a acestora;
}
\usage{
CRV.computeMargDen(X, Y, commonDensity)
}

\arguments{
  \item{X}{
    v.a. continua
  }
  \item{Y}{
    v.a. continua
  }
  \item{commonDensity}{
    Densitatea comuna a celor doua v.a. continue (functie de doua argumente).
  }
}
\details{
Functia printeaza densitatile marginale & conditionate corespunzatoare
fiecarei v.a. ;
}
\value{

}
\references{
https://www.rdocumentation.org/packages/Ryacas/versions/0.4.1
}
\author{
%%  ~~who you are~~
}
\note{
Functia utilizeaza Ryacas pentru utilizarea calculului simbolic.
"Ryacas is an R interface to the free yacas Computer Algebra System. Ryacas allows one to send R expressions, unprocessed yacas strings and certain other R objects to yacas process from R and get back the result. It also has facilities for manipulating yacas strings and R expressions destined for yacas processing.

It can be used for exact arithmetic, symbolic math, ASCII pretty printing and translating R to TeX."
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

CRV.computeMargDen(X, Y, com)
}
