VSURF.interp.default <- function(x, y, vars, nfor.interp=25, nsd=1, ...) {
  
  # vars: selected variables indices after thresholding step
  # nfor.interp: number of forests to estimate each model
  # nsd: number of standard deviation: the selected model leads to an OOB error
  # smaller than the min error + nsd * (sd of the min error)

  start <- Sys.time()
  
  # one forest run to determine the problem type: classification or regression
  rf <- randomForest(x=x, y=y, ntree=1, ...)
  if (rf$type=="classification") {
    type <- "classif"
  }
  if (rf$type=="regression") {
    type <- "reg"
  } 

  err.interp <- NULL
  sd.interp <- NULL
  
  nvars <- length(vars)
  n <- nrow(x)
  
  for (i in 1:nvars){
    rf <- NULL
    u <- vars[1:i]
    w <- as.matrix(x[,u])
    if (type=="classif") {
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ...)$err.rate[,1], n=1)
        }
      }
      else {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, mtry=i/3, ...)$err.rate[,1], n=1)
        }
      }
    }
    if (type=="reg") {
      if (i <= n) {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ...)$mse, n=1)
        }
      }
      else {
        for (j in 1:nfor.interp) {
          rf[j] <- tail(randomForest(x=w, y=y, ntree=1000, ...)$mse, n=1)
        }
      }
    }
    err.interp[i] <- mean(rf)
    sd.interp[i] <- sd(rf)
  }
  
  var.min <- which.min(err.interp)
  sd.min <- sd.interp[var.min]
  
  nvarselect <- min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
  varselect <- vars[1:nvarselect]

  cl <- match.call()
  cl[[1]] <- as.name("VSURF.interp")

  comput.time <- Sys.time()-start

  output <- list('varselect.interp'=varselect,
                 'err.interp'=err.interp,
                 'sd.min'=sd.min,
                 'num.varselect.interp'=nvarselect,
                 'varselect.thres' = vars,
                 'comput.time'=comput.time,
                 'call'=cl)
  class(output) <- "VSURF.interp"
  output
}
