VSURF <- function(x, y, ntree=500, nfor.thres=50, nmin=1, nfor.interp=25, nsd=1,
                  nfor.pred=25, nmj=1,
                  mtry=if (!is.factor(y)) max(floor(ncol(x)/3), 1)
                  else floor(sqrt(ncol(x))) ) {
  
  thres <- VSURF.thres(x=x, y=y, ntree=ntree, mtry=mtry, nfor.thres=nfor.thres, nmin=nmin)
  
  interp <- VSURF.interp(x=x, y=y, vars=thres$varselect.thres, nfor.interp=nfor.interp, nsd=nsd)
  
  pred <- VSURF.pred(x=x, y=y, err.interp=interp$err.interp,
                     varselect.interp=interp$varselect.interp, nfor.pred=nfor.pred, nmj=nmj)

  output <- list('varselect.thres'=thres$varselect.thres,
                 'imp.varselect.thres'=thres$imp.varselect.thres,
                 'min.thres'=thres$min.thres,
                 'ord.imp'=thres$ord.imp,
                 'ord.sd'=thres$ord.sd,
                 'mean.perf'=thres$mean.perf,
                 'varselect.interp'=interp$varselect.interp,
                 'err.interp'=interp$err.interp,
                 'sd.min'=interp$sd.min,
                 'varselect.pred'=pred$varselect.pred,
                 'err.pred'=pred$err.pred,
                 'mean.jump'=pred$mean.jump
                 )
}
