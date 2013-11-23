\name{swew}
\alias{swew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
startswith/endswith internal
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
swew(x, pattern, from)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
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
function (x, pattern, from) 
{
    if (length(x) != 1) {
        stop("x must be of length 1")
    }
    return(any(unlist(lapply(c(1:nchar(x)), function(i) {
        if (from == "s") Str = substr(x, 1, i)
        if (from == "e") Str = substr(x, i, nchar(x))
        return(Str == pattern)
    }))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
