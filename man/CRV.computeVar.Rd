\name{CRV.computeVar}
\alias{CRV.computeVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeVar
}
\description{
Calculul dispersiei unei v.a. continue.
}
\usage{
CRV.computeVar(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
obj -: v.a. continua construita cu ajutorul ContinuousRV::CRV.
}
}
\details{
Pentru determinarea valoriile variantei se utilieaza formula:\cr
\code{m <- CRV.computeEV(obj)\cr
    integrand <- function(x) {\cr
      (x - m)**2 * obj@fun(x)
    \cr}\cr

    result <- integrate(integrand, lower = obj@lo, upper = obj@hi)$value}
},\cr
unde m represinta media v.a.
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
X <- CRV(fun = function(x) {exp(1)**(-x**2/2)})
var <- CRV.computeVar(X)
}
