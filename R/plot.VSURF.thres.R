plot.VSURF.thres <- function(x, nvar.imp=NULL, nvar.sd=NULL, imp=TRUE, imp.sd=TRUE,
                             var.names=FALSE, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {input <- eval(x$call$x)}

  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)
  
  if (imp & imp.sd) {
    par(mfrow=c(1,2))
  }

  if (imp) {
      if (var.names) {
          plot(x$ord.imp$x[1:nvar.imp], type="l", xaxt="n", xlab="variables",
               ylab="VI mean", ...)
          axis(side=1, at=1:nvar.imp, labels=colnames(input[x$ord.imp$ix])[1:nvar.imp])
      }

      else {
          plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
               ylab="VI mean", ...)
      }

      abline(h=x$nmin * x$min.thres, col="red", ...)
  }

  if (imp.sd) {
      if (var.names) {
          plot(x$ord.sd[1:nvar.sd], type="l", xaxt="n", xlab="variables",
               ylab="VI standard deviation", ...)
          axis(side=1, at=1:nvar.sd, labels=colnames(input[x$ord.imp$ix])[1:nvar.sd])
      }          

      else {
          plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
               ylab="VI standard deviation", ...)
      }
      
      lines(x$pred.pruned.tree, type="s", col="green", ...)
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
  }
}
