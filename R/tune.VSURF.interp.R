tune.VSURF.interp <- function (x, nsd = 1, ...) {
  
  err.interp <- x$err.interp
  sd.min <- x$sd.min
  vars <- x$varselect.thres

  var.min <- which.min(err.interp)
  nvarselect <- min(which(err.interp <= (err.interp[var.min] + nsd * sd.min)))
  varselect <- vars[1:nvarselect]

  output <- list('varselect.interp' = varselect,
                 'err.interp' = err.interp,
                 'sd.min' = sd.min,
                 'num.varselect.interp'= length(varselect),
                 'varselect.thres' = vars)
}
