\name{iTable}
\alias{iTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Groups indices of a list or vector into n (almost) equal chunks:
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
iTable(x, n)
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
function (x, n) 
{
    x = seq_along(x)
    i = floor(seq(from = x[1], to = x[length(x)], length.out = n + 
        1))
    res = data.frame(from = i[1:(length(i) - 1)], to = i[2:length(i)])
    res$from[2:nrow(res)] = res$from[2:nrow(res)] + 1
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
