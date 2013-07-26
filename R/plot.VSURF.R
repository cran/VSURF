plot.VSURF <- function(x, ...) {

par(mfrow=c(2,2), las=1)

plot(x$ord.imp$x, type="l")
abline(h=x$nmin * x$min.thres, col="red")

plot(x$ord.sd, type="l")
lines(x$pred.pruned.tree, type="s", col="green")
abline(h=x$nmin * x$min.thres, col="red", lty = "dotted")

plot(x$err.interp*100, type="l")
abline(v=length(x$varselect.interp), col="red")

plot(x$err.pred*100, type="l")
}
