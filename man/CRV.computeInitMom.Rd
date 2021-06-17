\name{CRV.computeInitMom}
\alias{CRV.computeInitMom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeInitMom
}
\description{
Determina momentele initiale pana la ordinul 4 pentru o v.a. continua construita
utilizand ContinuousRV::CRV.
}
\usage{
CRV.computeInitMom(obj)
}
\arguments{
  \item{obj}{
  v.a. continua construita utilizand ContinuousRV::CRV.
}
}
\details{
\code{ fun <- obj@fun                                                       \cr
    for(i in 0 : 4) {                                                       \cr
      f <- function(x) {                                                    \cr
        x**i                                                                \cr
      }                                                                     \cr
      integrand <- function(x) {                                            \cr
        fun(x) * f(x)                                                       \cr
      }                                                                     \cr
      out <- integrate(integrand, lower = obj@lo, upper = obj@hi)$value     \cr
      result <- append(result, out)                                         \cr
    }}
}
\value{
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
moments <- CRV.computeInitMom(X)

library(MASS)
fractions(moments)
}
