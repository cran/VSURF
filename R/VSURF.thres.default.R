VSURF.thres.default <- function(x, y, ntree=2000, mtry=max(floor(ncol(x)/3), 1),
                        nfor.thres=50, nmin=1, ...) {

  # x: input
  # y: output
  # nfor.thres: number of forests to compute the mean importance of variables (IV)
  # nmin: thresholding parameter (if this procedure step keeps too much variables,
  # this value can be increased, e.g. to 3 or 5)

  start <- Sys.time()
  
  # one forest run to determine the problem type: classification or regression
  rf <- randomForest(x=x, y=y, ntree=1, ...)
  if (rf$type=="classification") {
    type <- "classif"
  }
  if (rf$type=="regression") {
    type <- "reg"
  } 

  # m: matrix with IV
  # perf: matrix with OOB errors
  m <- matrix(NA, nrow=nfor.thres, ncol=ncol(x))
  perf <- matrix(NA, nrow=nfor.thres, ncol=1)
  
  # if all forests have to be stored in memory, lines involving "rfmem" must be uncommented
  #rfmem=list()
  
  # filling of matrix m by running nfor.thres forests and keeping IV
  # filling of perf with the nfor.thres forests OOB errors
  if (type=="classif") {
    for (i in 1:nfor.thres){
      rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
      #rfmem=c(rfmem,list(rf))
      m[i,] <- rf$importance[, length(levels(y))+1]
      perf[i] <- tail(rf$err.rate[,1], n=1)
    }
  }
  if (type=="reg") {
    for (i in 1:nfor.thres){
      rf <- randomForest(x=x, y=y, ntree=ntree, mtry=mtry, importance=TRUE, ...)
      #rfmem=c(rfmem,list(rf))
      m[i,] <- rf$importance[, 1]
      perf[i] <- tail(rf$mse, n=1)
    }
  }
    
  # ord.imp contains the IV means in decreasing order
  ord.imp <- sort( colMeans(m), index.return=TRUE, decreasing=TRUE)
  
  # mean.perf contains the forests mean OOB error
  mean.perf <- mean(perf)
  
  # ord.sd contains IV standard deviations of all variables sorted according to ord.imp
  sd.imp <- apply(m, 2, sd)
  ord.sd <- sd.imp[ord.imp$ix]
    
  # particular case where x has only one variable
  s <- NULL
  if (ncol(as.matrix(x))==1) {
    s <- 1
  }
  else {
    p <- ncol(x)
    u <- 1:p
    u <- as.data.frame(u)
    
    # estimation of the standard deviations curve with CART (using "rpart" package)
    
    # construction of the maximal tree and search of optimal complexity
    tree <- rpart(ord.sd ~., data=u, control=rpart.control(cp=0, minsplit=2))
    d <- tree$cptable
    argmin.cp <- which.min(d[,4])
    
    # pruning
    pruned.tree <- prune(tree, cp=d[argmin.cp, 1])
    pred.pruned.tree <- predict(pruned.tree)
    
    # determination of the y-value of the lowest stair: this is the estimation
    # of the mean standard deviation of IV
    min.pred <- min(pred.pruned.tree)
    
    # thresholding: all variables with IV mean lower than min.pred are discarded
    w <- which(ord.imp$x < nmin*min.pred)
    
    if (length(w)==0) {
      s <- p
    }
    else {
      s <- min(w)-1
    }
  }
  
  # varselect: selected variables index
  # impvarselect: corresponding IV means
  varselect.thres <- ord.imp$ix[1:s]
  imp.varselect.thres <- ord.imp$x[1:s]
  
  cl <- match.call()
  cl[[1]] <- as.name("VSURF.thres")

  comput.time <- Sys.time()-start
  
  output <- list('varselect.thres'=varselect.thres,
                 'imp.varselect.thres'=imp.varselect.thres,
                 'min.thres'=min.pred,
                 'num.varselect.thres'=s,
                 'ord.imp'=ord.imp,
                 'ord.sd'=ord.sd,
                 'mean.perf'=mean.perf,
                 'pred.pruned.tree'=pred.pruned.tree,
                 'nmin' = nmin,
                 'comput.time'=comput.time,
                 'call'=cl)
  class(output) <- "VSURF.thres"
  output
}
