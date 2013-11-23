i2n <-
function(df, i) {
    out = vector()
    for (index in i) {
        out = c(out, names(df)[index])
    }
    return(out)
}
