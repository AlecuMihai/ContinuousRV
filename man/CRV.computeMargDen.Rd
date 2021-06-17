\name{CRV.computeMargDen}
\alias{CRV.computeMargDen}

\title{
CRV.computeMargDen
}
\description{
Determina densitatile marginale si conditionate a doua variable aleatoare
continue (CRV) folosindu-se de densitatea comuna a acestora;
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
    Densitatea comuna a celor doua v.a. continue.
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
X <- CRV(fun = function(x) {x}, lo = 0, hi = 1)
Y <- CRV(fun = function(x) {x}, lo = 0, hi = 2)
com <- function(x, y) {
  0.2 * (x + y + 1)
}

CRV.computeMargDen(X, Y, com)
}
