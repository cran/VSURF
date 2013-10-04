plot.VSURF <- function(x, nvar.imp=NULL, nvar.sd=NULL,...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {
    input <- eval(x$call$x)
  }

  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(mfrow=c(2,2), las=1, mar=c(4.5, 5, 1, 1)+0.1, tck=-0.02)

  plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
       ylab="VI mean", lwd=2)
  abline(h=x$nmin * x$min.thres, col="red", lwd=2)

  plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
       ylab="", lwd=2)
  mtext(text="VI standard deviation", side=2, line=4, las=0,
        cex=par("cex"))
  lines(x$pred.pruned.tree, type="s", col="green", lwd=2)
  abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", lwd=2)

  plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
       ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), lwd=2)
  axis(side=1, at=1:length(x$varselect.thres),
       labels=colnames(input[x$varselect.thres]))
  abline(v=length(x$varselect.interp), col="red", lwd=2)
       
  plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
       ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), lwd=2)
  axis(side=1, at=1:length(x$varselect.pred),
       labels=colnames(input[x$varselect.pred]))
}
