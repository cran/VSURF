VSURF.interp.tune <- function (res.interp, nsd = 1) {
  
  err.interp <- res.interp$err.interp
  sd.min <- res.interp$sd.min
  vars <- res.interp$varselect.thres

  var.min <- which.min(err.interp)
  nvarselect <- min(which(err.interp <= (err.interp[var.min] + nsd * sd.min)))
  varselect <- vars[1:nvarselect]

  output <- list('varselect.interp' = varselect,
                 'err.interp' = err.interp,
                 'sd.min' = sd.min,
                 'varselect.thres' = vars)
}
