\name{VSURF.pred}
\alias{VSURF.pred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prediction step of VSURF for supervised classification problems
}
\description{
Prediction step refines the selection of intepretation step \code{\link{VSURF.interp}} by eliminating redundancy in the set of variables selected, for prediction prupose.This is the third step of the \code{\link{VSURF}} function for supervised classification problems.
}
\usage{
VSURF.pred(x, y, err.interp, varselect.interp, nfor.pred = 25, nmj = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame or a matrix of predictors, the columns represent the variables.
}
  \item{y}{
A response vector (must be a factor for classification problems and
numeric for regression ones).
}
  \item{err.interp}{
A vector of the mean OOB error rates of the embedded random forests models build during interpretation step (value \code{err.interp} of function \code{\link{VSURF.interp}}).
}
  \item{varselect.interp}{
A vector of indices of variables selected after interpretation step.
}
  \item{nfor.pred}{
Number of forests grown.
}
  \item{nmj}{
Number of times the mean jump is multiplied. See details below.
}
}
\details{
\code{nfor.pred} embedded random forests models are grown, starting with the random forest build with only the most important variable. Variables are added to the model in a stepwise manner. The mean jump value \code{mean.jump} is calculated using variables that have been left out by interpretation step, and is set as the mean absolute difference between mean OOB errors of one model and its first following model. Hence a variable is included in the model if the mean OOB error decrease is larger than \code{nmj} * \code{mean.jump}.
}
\value{
An object of class \code{VSURF.pred}, which is a list with the following components:
 \item{varselect.pred}{
 A vector of indices of variables selected after "prediction step".
 }
 \item{err.pred}{
 A vector of the mean OOB error rates of the random forests models build during the "prediction step".
 }
 \item{mean.jump}{
 The mean jump value computed during the "prediction step".
 }
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
\code{\link{VSURF}}
}
\examples{
data(iris)
iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
iris.interp <- VSURF.interp(x=iris[,1:4], y=iris[,5], vars=iris.thres$varselect.thres,
                            nfor.interp=10)
iris.pred <- VSURF.pred(x=iris[,1:4], y=iris[,5], err.interp=iris.interp$err.interp,
varselect.interp=iris.interp$varselect.interp, nfor.pred=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
