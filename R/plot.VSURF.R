plot.VSURF <- function(x, nvar.imp=NULL, nvar.sd=NULL, var.names=FALSE, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {input <- eval(x$call$x)}

  if (is.null(nvar.imp)) {nvar.imp <- length(x$ord.imp$x)}
  if (is.null(nvar.sd)) {nvar.sd <- length(x$ord.sd)}
  
  par(mfrow=c(2,2), mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$ord.imp$x[1:nvar.imp], type="l", xaxt="n", xlab="variables",
           ylab="VI mean", ...)
      axis(side=1, at=1:nvar.imp, labels=colnames(input[x$ord.imp$ix])[1:nvar.imp])
      abline(h=x$nmin * x$min.thres, col="red")
      
      plot(x$ord.sd[1:nvar.sd], type="l", xaxt="n", xlab="variables",
           ylab="VI standard deviation", ...)
      axis(side=1, at=1:nvar.sd, labels=colnames(input[x$ord.imp$ix])[1:nvar.sd])
      lines(x$pred.pruned.tree, type="s", col="green")
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
      
      plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.thres),
           labels=colnames(input[x$varselect.thres]))
      abline(v=length(x$varselect.interp), col="red", ...)
       
      plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.pred),
           labels=colnames(input[x$varselect.pred]))
  }

  else {  
      plot(x$ord.imp$x[1:nvar.imp], type="l", xlab="variables",
           ylab="VI mean", ...)
      abline(h=x$nmin * x$min.thres, col="red")

      plot(x$ord.sd[1:nvar.sd], type="l", xlab="variables",
           ylab="VI standard deviation", ...)
      lines(x$pred.pruned.tree, type="s", col="green")
      abline(h=x$nmin * x$min.thres, col="red", lty = "dotted", ...)
      
      plot(x$err.interp, type="l", xlab="nested models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
      abline(v=length(x$varselect.interp), col="red", ...)
       
      plot(x$err.pred, type="l", xlab="predictive models",
           ylab="OOB error",
           ylim=c(min(x$err.interp, x$err.pred)*0.95, max(x$err.interp)*1.05), ...)
  }
}
