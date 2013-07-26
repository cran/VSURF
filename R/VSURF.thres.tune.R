VSURF.thres.tune <- function (res.thres, nmin = 1) {
  
  ord.imp <- res.thres$ord.imp
  ord.sd <- res.thres$ord.sd
  min.pred <- res.thres$min.thres
  mean.perf <- res.thres$mean.perf
  pred.pruned.tree <- res.thres$pred.pruned.tree
  
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
