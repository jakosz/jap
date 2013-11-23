iTable <-
function(x, n) {
    x = seq_along(x)
    i = floor(seq(from = x[1], to = x[length(x)], length.out = n+1))
    res = data.frame(from = i[1:(length(i)-1)], to = i[2:length(i)])
    res$from[2:nrow(res)] = res$from[2:nrow(res)] + 1
    return(res)
}
