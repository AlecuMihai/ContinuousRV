\name{CRV.computeCentrMom}
\alias{CRV.computeCentrMom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeCentrMom
}
\description{
Determina momentele centrate pana la ordinul 4 pentru o v.a. continua
}
\usage{
CRV.computeCentrMom(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
V.a. continua construita cu ajutorul ContinueRV::CRV.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{mom_{1:4}}
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
moments <- CRV.computeCentrMom(X)

library(MASS)
fractions(moments)
}
