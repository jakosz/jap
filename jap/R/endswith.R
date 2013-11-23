`%endswith%` <-
function(x, pattern) {
    return(any(unlist(lapply(x, swew, pattern, 'e'))))
}
