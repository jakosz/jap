n2i <-
function(df, n) {
    out = vector()
    for (name in n) {
        out = c(out, which(names(df) == name))
    }
    return(out)
}
