\name{ni}
\alias{ni}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts column indices to names or names to indices 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ni(df, Vector, what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{Vector}{
%%     ~~Describe \code{Vector} here~~
}
  \item{what}{
%%     ~~Describe \code{what} here~~
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
function (df, Vector, what) 
{
    if (what == "i") {
        if (is.numeric(Vector)) {
            return(Vector)
        }
        else {
            return(n2i(df, Vector))
        }
    }
    if (what == "n") {
        if (is.numeric(Vector)) {
            return(i2n(df, Vector))
        }
        else {
            return(Vector)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
