plot.VSURF.thres <- function(x, nvar.imp=NULL, nvar.sd=NULL, imp=TRUE, imp.sd=TRUE,...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {input <- eval(x$call$x)}

  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(las=1, mar=c(4.5, 5, 1, 1)+0.1, tck=-0.02)
  
  if (imp & imp.sd) {
    par(mfrow=c(1,2))
  }

  if (imp) {
    plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
         ylab="VI mean", lwd=2)
    abline(h=x$nmin * x$min.thres, col="red", lwd=2)
  }

  if (imp.sd) {
    plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
         ylab="", lwd=2)
    mtext(text="VI standard deviation", side=2, line=4, las=0)
    lines(x$pred.pruned.tree, type="s", col="green", lwd=2)
    abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", lwd=2)
  }
}
