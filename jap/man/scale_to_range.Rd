\name{scale_to_range}
\alias{scale_to_range}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scales a numeric vector and transforms it so it has min at 'from' and max at 'to'
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scale_to_range(x, from = 1, to = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
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
function (x, from = 1, to = 100) 
{
    x = scale(x)[, 1]
    x = x + abs(min(x))
    x = x * ((to - from)/max(x))
    return(x + from)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
