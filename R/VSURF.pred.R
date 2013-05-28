VSURF.pred <-function(x, y, err.interp, varselect.interp, nfor.pred=25, nmj=1){
  
  # err.interp: interpretation models errors
  # varselect.interp: interpretation variables indices
  # nmj: number of mean jump: addition of a variable if it gives an error reduction of at
  # least nmj * mean jump value
  
  # one forest run to determine the problem type: classification or regression
  rf <- randomForest(x=x, y=y, ntree=1)
  if (rf$type=="classification") {
    type <- "classif"
  }
  if (rf$type=="regression") {
    type <- "reg"
  } 

  k <- length(err.interp)
  l <- length(varselect.interp)
  
  if (k==l) {
    print('unable to do the prediction step, because the interpretation step did not
eliminate variables')
    var.select <- NULL
    err.pred <- NULL
    mean.jump <- NULL
  }
  else {
    
    # mean jump calculation
    s=NULL
    for (i in l:(k-1)){
      s <- c(s, abs(err.interp[i+1] - err.interp[i]) )
    }
    mean.jump <- mean(s)
    
    # comparison between the error with the variable and the precedent error
    # and test of the addition of the variable
    varselect.pred <- varselect.interp[1]
    u <- varselect.pred
    w <- as.matrix(x[,u])
    if (type=="classif") {
      err.pred <- mean( replicate(nfor.pred, randomForest(w, y)$err.rate[500]) )
    }
    if (type=="reg") {
      err.pred <- mean( replicate(nfor.pred, randomForest(w, y)$mse[500]) )
    }
    t <- err.pred
    
    if (l>1) { 
      for (i in 2:l){
        u <- c(varselect.pred, varselect.interp[i])
        w <- as.matrix(x[,u])
        if (type=="classif") {
          z <- mean( replicate(nfor.pred, randomForest(w, y)$err.rate[500]) )
        }
        if (type=="reg") {
          z <- mean( replicate(nfor.pred, randomForest(w, y)$mse[500]) )
        }
        if ((t-z) > nmj*mean.jump){
          varselect.pred <- c(varselect.pred, varselect.interp[i])
          err.pred <- c(err.pred, z)
        }
        t <- z
      }
    }
  }
  
  output <- list('varselect.pred'=varselect.pred,
                 'err.pred'=err.pred,
                 'mean.jump'=mean.jump)
}
