summary.VSURF <- function(object, ...) {
  
  cat(paste("\n VSURF computation time:", round(object$comput.time, 1),
            attributes(object$comput.time)$units, "\n", sep=" ")
      )
  
  cat(paste("\n VSURF selected: \n",
            "\t", object$num.varselect.thres, " variables at thresholding step \n",
            "\t", object$num.varselect.interp, " variables at interpretation step \n",
            "\t", object$num.varselect.pred, " variables at prediction step \n", sep="")
      )
}
