summary.VSURF <- function(object, ...) {
  
  cat(paste("\n VSURF computation time:", round(object$overall.time, 1),
            attributes(object$overall.time)$units, "\n", sep=" ")
      )
  
  cat(paste("\n VSURF selected: \n",
            "\t", object$nums.varselect[1], " variables at thresholding step ",
            "(in ", round(object$comput.times[[1]], 1), " ",
            attributes(object$comput.times[[1]])$units, ")", "\n",
            "\t", object$nums.varselect[2], " variables at interpretation step ",
            "(in ", round(object$comput.times[[2]], 1), " ",
            attributes(object$comput.times[[2]])$units, ")", "\n",
            "\t", object$nums.varselect[3], " variables at prediction step ",
            "(in ", round(object$comput.times[[3]], 1), " ",
            attributes(object$comput.times[[3]])$units, ")", "\n",
            sep="")
      )
}
