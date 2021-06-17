\name{CRV.computeP}
\alias{CRV.computeP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeP
}
\description{
Calculeaza probabilitatea unor evenimente.
}
\usage{
CRV.computeP(obj, x, y = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
v.a. continua construita utilizant ContinuousRV::CRV.
}
  \item{x}{
Expresia/intervalul de probabilitate.
}
  \item{\dots}{
}
}
\details{
In cazul in care \code{_lower_bound < X@lo | _upper_bound > X@hi} functia arunca o eroare si opreste executia.
}
\value{
  \item{p} {
    Probabilitatea evenimentului introdus.
  }
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
CRV.computeP(X, X%<%3)
CRV.computeP(X, c(X@lo, 1/2))
}
