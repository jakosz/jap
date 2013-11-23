swew <-
function(x, pattern, from) {
    if (length(x) != 1) {
        stop('x must be of length 1')
    }
    return(any(unlist(lapply(c(1:nchar(x)), 
                             function(i) {
                                 if (from == 's') Str = substr(x, 1, i) 
                                 if (from == 'e') Str = substr(x, i, nchar(x))
                                 return(Str == pattern)
                             })))
    )
}
