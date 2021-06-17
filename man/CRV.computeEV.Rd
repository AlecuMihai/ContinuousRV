\name{CRV.computeEV}
\alias{CRV.computeEV}
\title{
CRV.computeEV
}
\description{
Determina media (expected value) unei v.a. continue.
}
\usage{
CRV.computeEV(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
  v.a. continua construita cu ajutorul ContinuousRV::CRV.
}
}
\details{
Pentru a determina e.v. este utilizata formula:\cr
\code{
result <- integrate(f.g, lower = obj@lo, upper = obj@hi)
}\cr
unde f.g este \code{f.g(x) = x * obj@fun(x)}
}
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
ev <- CRV.computeEV(X)
}
