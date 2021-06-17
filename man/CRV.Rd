\name{CRV}
\alias{CRV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV
}
\description{
Constructor pentru o v.a. continua.
}
\usage{
CRV(fun, lo, hi)
}
\arguments{
  \item{fun}{
    Functia specifica v.a. continua - aceastea este considerata nenula doar pe
    intervalul cuprins intre {_lower_bound, _upper_bound}.
  }
  \item{lo}{
    _lower_bound
  }
  \item{hi}{
    _upper_bound
  }
}
\details{
In cazul in care cele doua valori (lo, hi) nu sunt introduse de utilizator,
iau implicit valorile (-Inf, Inf).
}
\value{
}
\references{
https://programmingpages.wordpress.com/2014/08/31/creating-classes-in-r/

https://stat.ethz.ch/R-manual/R-devel/library/methods/html/setClass.html
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
X <- CRV(fun = function(x) {(exp)**(-x**2/2)}, lo = -1, hi = 1)
print(X)
