\name{CRV.computeMean}
\alias{CRV.computeMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeMean
}
\description{
Calculul mediei unde X este o v.a. continua cunoscuta, iar g este o functie continua.
}
\usage{
CRV.computeMean(obj, g)
}
\arguments{
  \item{obj}{
  obj -: v.a. continua construita cu ajutorul ContinuousRV::CRV.
}
  \item{g}{
  Functie continua.
}
\details{
  Pentru determinarea mediei este determinata functia                        \cr
                                                                             \cr
  \code{f.g(x) = obj@fun(x) * g(x)}                                          \cr
                                                                             \cr
  nenula pe intervalul
  \code{(obj@_lower_bound, obj@_upper_bound)},                               \cr
  care mai apoi este integrata pe   acelasi interval.
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
X <- CRV(fun = function(x) {x**2}, lo = -1/2, hi = 1/2)
g <- function(x) {x - 20}

m <- CRV.computeMean(X, g)
