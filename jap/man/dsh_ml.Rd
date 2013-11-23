\name{dsh_ml}
\alias{dsh_ml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dsh multiline handler
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dsh_ml(uInput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uInput}{
%%     ~~Describe \code{uInput} here~~
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
function (uInput) 
{
    savehistory()
    while (is.ml(uInput)) {
        nInput = readline(prompt = "dsh+ ")
        write(nInput, ".Rhistory", append = TRUE)
        loadhistory()
        uInput = ps(uInput, nInput, sep = "\n")
    }
    return(uInput)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
