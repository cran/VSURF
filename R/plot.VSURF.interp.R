plot.VSURF.interp <- function(x, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {
    input <- eval(x$call$x)
  }
  
  par(las=1, mar=c(4.5, 5, 1, 1)+0.1, tck=-0.02)

  plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
       ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), lwd=2)
  axis(side=1, at=1:length(x$varselect.thres),
       labels=colnames(input[x$varselect.thres]))
  abline(v=length(x$varselect.interp), col="red", lwd=2)
}
