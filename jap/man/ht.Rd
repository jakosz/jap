\name{ht}
\alias{ht}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
head & tail
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ht(x, n = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (x, n = 6) 
{
    if (class(x) \%in\% c("matrix", "data.frame")) {
        lFun = nrow
        lNam = "rows"
    }
    else {
        lFun = length
        lNam = "elements"
    }
    print(head(x, n))
    writeLines(ps("\n.. [ ", lFun(x), " ", lNam, " total ] ..\n"))
    print(tail(x, n))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
