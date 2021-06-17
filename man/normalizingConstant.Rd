\name{normalizingConstant}
\alias{normalizingConstant}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
normalizingConstant
}
\description{
Functia returneaza constanta de normalizare k.
}
\usage{
normalizingConstant(function, _lower_bound, _upper_bound)
}
\arguments{
  \item{function}{

  Functia pentru care urmeaza sa fie determinata constanta de normalizare.
  Functia se considera a fi nenula pe intervalul _lower_bound, _upper_bound.

            default._lower_bound = -Inf

            default._upper_bound =  Inf;
}
  \item{fun.lower}{
  _lower.bound
  }
  \item{fun.upper}{
  _upper.bound
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\arguments{
  \item{k}{
  constanta de normalizare
  }
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
x <- normalizingConstant(function(x){x + 1}, 0, 4)
}
