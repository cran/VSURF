tune.VSURF.thres <- function (x, nmin = 1, ...) {
  
  ord.imp <- x$ord.imp
  ord.sd <- x$ord.sd
  min.pred <- x$min.thres
  mean.perf <- x$mean.perf
  pred.pruned.tree <- x$pred.pruned.tree
  
  w <- which(ord.imp$x < nmin * min.pred)
  if (length(w) == 0) {
    s <- length(ord.sd)
  }
  else {
    s <- min(w)-1
  }
  
  varselect.thres <- ord.imp$ix[1:s]
  imp.varselect.thres <- ord.imp$x[1:s]
  
  output <- list('varselect.thres' = varselect.thres,
                 'imp.varselect.thres' = imp.varselect.thres, 
                 'min.thres' = min.pred,
                 'num.varselect' = s,
                 'ord.imp' = ord.imp,
                 'ord.sd' = ord.sd,
                 'mean.perf' = mean.perf,
                 'pred.pruned.tree' = pred.pruned.tree)
}
