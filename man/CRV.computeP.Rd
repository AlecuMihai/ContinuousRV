\name{CRV.computeP}
\alias{CRV.computeP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRV.computeP
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CRV.computeP(obj, x, y = NULL)
}
\usage{
\%<\%(X, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
new("standardGeneric", .Data = function (obj, x, y = NULL)
standardGeneric("CRV.computeP"), generic = structure("CRV.computeP", package = "ContinuousRV"),
    package = "ContinuousRV", group = list(), valueClass = character(0),
    signature = c("obj", "x", "y"), default = new("derivedDefaultMethod",
        .Data = function (obj, x, y = NULL)
        {
        }, target = new("signature", .Data = "ANY", names = "obj",
            package = "methods"), defined = new("signature",
            .Data = "ANY", names = "obj", package = "methods"),
        generic = structure("CRV.computeP", package = "ContinuousRV")),
    skeleton = (new("derivedDefaultMethod", .Data = function (obj,
        x, y = NULL)
    {
    }, target = new("signature", .Data = "ANY", names = "obj",
        package = "methods"), defined = new("signature", .Data = "ANY",
        names = "obj", package = "methods"), generic = structure("CRV.computeP", package = "ContinuousRV")))(obj,
        x, y))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
