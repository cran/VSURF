VSURF.default <- function(x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1),
                  nfor.thres=50, nmin=1, nfor.interp=25, nsd=1, nfor.pred=25, nmj=1, ...) {

  start <- Sys.time()
  
  thres <- VSURF.thres(x=x, y=y, ntree=ntree, mtry=mtry,
                       nfor.thres=nfor.thres, nmin=nmin, ...)
  
  interp <- VSURF.interp(x=x, y=y, vars=thres$varselect.thres,
                         nfor.interp=nfor.interp, nsd=nsd, ...)
  
  pred <- VSURF.pred(x=x, y=y, err.interp=interp$err.interp,
                     varselect.interp=interp$varselect.interp,
                     nfor.pred=nfor.pred, nmj=nmj, ...)

  cl <- match.call()
  cl[[1]] <- as.name("VSURF")

  overall.time <- Sys.time()-start

  output <- list('varselect.thres'=thres$varselect.thres,
                 'varselect.interp'=interp$varselect.interp,
                 'varselect.pred'=pred$varselect.pred,
                 'nums.varselect'=c(thres$num.varselect.thres,
                   interp$num.varselect.interp,
                   pred$num.varselect.pred),
                 'imp.varselect.thres'=thres$imp.varselect.thres,
                 'min.thres'=thres$min.thres,
                 'ord.imp'=thres$ord.imp,
                 'ord.sd'=thres$ord.sd,
                 'mean.perf'=thres$mean.perf,
                 'pred.pruned.tree' = thres$pred.pruned.tree,
                 'err.interp'=interp$err.interp,
                 'sd.min'=interp$sd.min,
                 'err.pred'=pred$err.pred,
                 'mean.jump'=pred$mean.jump,
                 'nmin' = nmin,
                 'nsd' = nsd,
                 'nmj' = nmj,
                 'overall.time'=overall.time,
                 'comput.times'=list(thres$comput.time, interp$comput.time, pred$comput.time),
                 'call'=cl)
  class(output) <- "VSURF"
  output
}
