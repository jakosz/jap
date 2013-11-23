splt <-
function(x, split = '', ...) {
    return(unlist(strsplit(x, split = split, ...)))
}
