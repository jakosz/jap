`%startswith%` <-
function(x, pattern) {
    return(any(unlist(lapply(x, swew, pattern, 's'))))
}
