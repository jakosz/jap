xTable <-
function(x, n) {
    from = seq(from = 1, to = len(x), by = n)
    to = c(from[2:len(from)] - 1, len(x))
    return(data.frame(from = from, to = to))
}
