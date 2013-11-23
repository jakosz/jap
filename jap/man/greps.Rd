\name{greps}
\alias{greps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
grep with logical output, for use in conditioning;
no match in grep gives integer(0) which yields error when used in if()
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
greps(pattern, x, fixed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
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
function (pattern, x, fixed = FALSE) 
{
    if (length(grep(pattern, x, fixed = fixed)) != 0) {
        return(1)
    }
    else {
        return(0)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
