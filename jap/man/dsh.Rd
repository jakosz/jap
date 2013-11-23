\name{dsh}
\alias{dsh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
R interactive shell used for its side effects.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dsh(scripts = NULL, plugins = c("ml", "vi"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scripts}{
%%     ~~Describe \code{scripts} here~~
}
  \item{plugins}{
%%     ~~Describe \code{plugins} here~~
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
function (scripts = NULL, plugins = c("ml", "vi")) 
{
    data(dsh_scripts)
    savehistory()
    while (TRUE) {
        if (!is.null(scripts)) {
            for (script in scripts) {
                try(source(script))
            }
        }
        uInput = readline(prompt = "dsh> ")
        write(uInput, ".Rhistory", append = TRUE)
        loadhistory()
        if (!is.null(plugins)) {
            for (plugin in plugins) {
                uInput = try(get(ps("dsh_", plugin))(uInput), 
                  silent = TRUE)
                if (class(uInput) == "try-error") {
                  write(ps("Error in executing plugin ", plugin), 
                    stdout())
                  break
                }
            }
        }
        res = try(eval(parse(text = uInput)), silent = TRUE)
        if (class(res) != "try-error") {
            if (!is.null(res)) 
                print(res)
        }
        else {
            write(ps("Error: ", attr(res, "condition")$message), 
                stdout())
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
