\name{isPDF}
\alias{isPDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isPDF
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isPDF()
}
%- maybe also 'usage' for other objects documented here.
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
function ()
{
    out <- tryCatch({
        text <- readline("Introdu functia: ")
        fun.expression = parse(text = text)
        isPos <- tryCatch({
            function.deriv <- deriv(fun.expression, "x")
            f <- function(x) {
            }
            body(f) <- function.deriv
            f.roots <- uniroot(f, lower = -1, upper = 1)$root
            f.solutions <- f(f.roots)
        }, warning = function(cons) {
        }, error = function(cond) {
            sprintf("Error while computing if function is either positive or negative;\n\n            Positivity/negativity could not be determined!")
            return(T)
        })
        if (isPos) {
            integrand <- function(x) {
            }
            body(integrand) <- fun.expression
            result <- integrate(integrand, lower = -Inf, upper = Inf)$value
            if (as.integer(result) == 1) {
                return(T)
            }
            else {
                return(F)
            }
        }
    }, error = function(cond) {
        print("Error while parsing text to function")
        return(F)
    }, warning = function(cond) {
    })
    return(out)
  }
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
