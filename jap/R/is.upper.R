is.upper <-
function(x) {
    return(sapply(x, function(s) all(sapply(splt(s), function(l) l == toupper(l)))))
}
