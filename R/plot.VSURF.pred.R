plot.VSURF.pred <- function(x, var.names=FALSE, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {
    input <- eval(x$call$x)
  }
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$err.pred, type="l", xaxt="n", xlab="predictive models",
           ylab="OOB error", ylim=c(0, max(x$err.pred)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.pred),
           labels=colnames(input[x$varselect.pred]))
  }

  else {
      plot(x$err.pred, type="l", xlab="predictive models",
           ylab="OOB error", ylim=c(0, max(x$err.pred)*1.05), ...)
  }
}   
