plot_empty <-
function(xlim=c(-1,1),ylim=c(-1,1),bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         type='n',layout = NULL,...) {
    if (!is.null(layout)) {
        xlim = c(min(layout[,1]), max(layout[,1]))
        ylim = c(min(layout[,2]), max(layout[,2]))
    }
    plot(0,xlim=xlim,ylim=ylim,bty=bty,xaxt=xaxt,yaxt=yaxt,xlab=xlab,ylab=ylab,
         type=type,layout=layout,...)
}
