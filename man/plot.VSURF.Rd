\name{plot}

\alias{plot.VSURF}
\alias{plot.VSURF.thres}
\alias{plot.VSURF.interp}
\alias{plot.VSURF.pred}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Plot of VSURF results
}

\description{
  This function plots 4 graphs illustrating VSURF results.
}

\usage{
\method{plot}{VSURF}(x, ...)
\method{plot}{VSURF.thres}(x, ...)
\method{plot}{VSURF.interp}(x, ...)
\method{plot}{VSURF.pred}(x, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An object of class \code{VSURF}, \code{VSURF.thres},
    \code{VSURF.interp} or \code{VSURF.pred}, which is the result of the
   \code{\link{VSURF}} function (or resp. \code{\link{VSURF.thres}},
   \code{\link{VSURF.interp}} or \code{\link{VSURF.pred}}).
 }
 \item{\dots}{
   Not used.
  }
}

\details{
  The 2 graphs of the top row correspond to the "thresholding step" (and
  only these 2 graphs are plotted by the \code{plot.VSURF.thres} function).
  The top left graph plots the mean variable importance in decreasing
  order (black curve). The red horizontal line represent the value of the threshold.
  The top right graph plots the standard deviation of variable importance with
  variables ordered according to their mean variable importance in
  decreasing order (black curve). The green line represents the predictions given by a
  CART tree fitted to the black curve (the standard
  deviations). Finally, the dotted horizontal red line represents the
  minimum value of the CART predictions, which actually is the value of
  the threshold.
  
  The bottom left graph corresponds to the "interpretation step" (and
  only this graph is plotted by the \code{plot.VSURF.interp} function). It
  plots the mean OOB error rate of embedded random forests models (from
  the one with only one variable as predictor, to the one with all
  variables kept after the "thresholding step"). The vertical red line
  indicates the retained model.
  
  The bottom right graph corresponds to the "predicton step" (and
  only this graph is plotted by the \code{plot.VSURF.pred} function). It plots
  the mean OOB error rate of embedded random forests models (the
  difference, here, being that variables are added to the model in a
  step-wise manner). The retained model is the final one.
}

\references{
Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010), \emph{Variable selection using random forests}, Pattern Recognition Letters 31(14), 2225-2236
}

\author{
Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{VSURF}}, \code{\link{summary.VSURF}}
}

\examples{
\dontrun{
data(iris)
iris.vsurf <- VSURF(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20,
                    nfor.interp=10, nfor.pred=10)
plot(iris.vsurf)

# A more interesting example with toys data (see \code{\link{toys}})
# (a few minutes to execute) and intermediate functions
data(toys)
toys.vsurf <- VSURF(x=toys$x, y=toys$y)
plot(toys.vsurf)
toys.thres <- VSURF.thres(x=toys$x, y=toys$y)
plot(toys.thres)
toys.interp <- VSURF.interp(x=toys$x, y=toys$y, vars=toys.thres$varselect.thres)
par(mfrow=c(1,1))
plot(toys.interp)
toys.pred <- VSURF.pred(x=toys$x, y=toys$y, err.interp=toys.interp$err.interp,
                        varselect.interp=toys.interp$varselect.interp)
plot(toys.pred)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
