\name{plot_empty}
\alias{plot_empty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots an empty plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_empty(xlim = c(-1, 1), ylim = c(-1, 1), bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", layout = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{bty}{
%%     ~~Describe \code{bty} here~~
}
  \item{xaxt}{
%%     ~~Describe \code{xaxt} here~~
}
  \item{yaxt}{
%%     ~~Describe \code{yaxt} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{layout}{
%%     ~~Describe \code{layout} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (xlim = c(-1, 1), ylim = c(-1, 1), bty = "n", xaxt = "n", 
    yaxt = "n", xlab = "", ylab = "", type = "n", layout = NULL, 
    ...) 
{
    if (!is.null(layout)) {
        xlim = c(min(layout[, 1]), max(layout[, 1]))
        ylim = c(min(layout[, 2]), max(layout[, 2]))
    }
    plot(0, xlim = xlim, ylim = ylim, bty = bty, xaxt = xaxt, 
        yaxt = yaxt, xlab = xlab, ylab = ylab, type = type, layout = layout, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
