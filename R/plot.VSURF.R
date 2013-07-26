plot.VSURF <- function(x, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {
    input <- eval(x$call$x)
  }
  
  par(mfrow=c(2,2), las=1, mar=c(4.5, 5, 1, 1)+0.1, tck=-0.02)

  plot(x$ord.imp$x, type="l", xlab="variables", lwd=2)
  abline(h=x$nmin * x$min.thres, col="red", lwd=2)

  plot(x$ord.sd, type="l", xlab="variables", lwd=2)
  lines(x$pred.pruned.tree, type="s", col="green", lwd=2)
  abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", lwd=2)

  plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
       ylim=c(0, max(x$err.interp)*1.05), lwd=2)
  axis(side=1, at=1:length(x$varselect.thres),
       labels=colnames(input[x$varselect.thres]))
  abline(v=length(x$varselect.interp), col="red", lwd=2)
       
  plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
       ylim=c(0, max(x$err.interp)*1.05), lwd=2)
  axis(side=1, at=1:length(x$varselect.pred),
       labels=colnames(input[x$varselect.pred]))
}
