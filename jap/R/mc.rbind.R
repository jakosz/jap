mc.rbind <-
function(ldf) {
    itable = iTable(ldf, detectCores())
    res = mclapply(c(1:detectCores()), 
                   function(x) {
                       do.call('rbind', ldf[itable$from[x]:itable$to[x]])
                   })
    res = do.call('rbind', res)
    return(res)
}
