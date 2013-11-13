plot.VSURF.interp <- function(x, var.names=FALSE, ...) {

  if (!is.null(x$terms)) {
    input <- model.frame(terms(reformulate(attributes(x$terms)$term.labels)),
                         eval(as.expression(x$call$data)))
  }

  else {
    input <- eval(x$call$x)
  }
  
  par(mar=c(5, 4, 2, 2)+0.1, tck=-0.02)

  if (var.names) {
      plot(x$err.interp, type="l", xaxt="n", xlab="nested models",
           ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), ...)
      axis(side=1, at=1:length(x$varselect.thres),
           labels=colnames(input[x$varselect.thres]))
  }

  else {
      plot(x$err.interp, type="l", xlab="nested models",
           ylab="OOB error", ylim=c(0, max(x$err.interp)*1.05), ...)
  }

  abline(v=length(x$varselect.interp), col="red", ...)

}
