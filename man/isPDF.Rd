\name{isPDF}
\alias{isPDF}
\title{
isPDF
}
\description{
Determina daca o functie nenula pe un interval (_lower_bound, _upper_bound)
este sau nu densitate de probabilitate.
}
\usage{
isPDF(fun, fun.lower, fun.upper)
}
\arguments{
  \item{fun} {
  Functia.class -: function
  }
  \item{fun.lower} {
  _lower_bound.class -: numeric                                             \cr

  In cazul in care argumentul nu este specificat, acesta ia implicit valoarea -Inf.
  }
  \item{fun.upper} {
  _upper_bound.class -: numeric
                                                                            \cr
  In cazul in care argumentul nu este specificat, acesta ia implicit valoarea  Inf.
  }
}
\details{

Pentru determinarea proprietatii se verifica urmatoarele:                   \cr
  1. Functia introdusa trebuie sa fie pozitiva.
  In cazul in care intervalul pe care functia este nenula = (-Inf, Inf),
  verificarea are loc pe (-1000, 1000);                                     \cr
  2. Se verifica daca                                                       \cr
  \code{integrate(fun, lower = _lower_bouund, upper = _upper_bound) == 1};  \cr

  In cazul in care (1), (2) verifica conditiile, aceasta este declarata ca fiind PDF si functia
  intoarce valoare \code{T}, altfel \code{F}.
}
\value{
  \item {
  (T/F)
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
isPDF(fun = function(x) {exp(1)**(-x**2/2)})
isPDF(fun = function(x) {x**2},
      fun.lower = -2.375,
      fun.upper =  8.355)
}
