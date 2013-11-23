greps <-
function(pattern, x, fixed = FALSE) {
    if (length(grep(pattern, x, fixed = fixed)) != 0) {
        return(1)
    } else {
        return(0)
    }
}
