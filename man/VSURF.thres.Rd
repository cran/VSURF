\name{VSURF.thres}
\alias{VSURF.thres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thresholding step of VSURF for supervised classification problems
}
\description{
Thresholding step is dedicated to roughly eliminate irrelevant variables a the dataset. This is the first step of the \code{\link{VSURF}} function for supervised classification problems. For refined variable selection, see the VSURF other steps: \code{\link{VSURF.interp}} and \code{\link{VSURF.pred}}.
}
\usage{
VSURF.thres(x, y, ntree=500, nfor.thres=50, nmin=1,
mtry=if (!is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A data frame or a matrix of predictors, the columns represent the variables.
}
  \item{y}{
A response vector (must be a factor for classification problems and
numeric for regression ones).}
  \item{ntree}{
Number of trees in each forest grown. Standard \code{randomForest} parameter.
}
  \item{nfor.thres}{
Number of forests grown.
}
  \item{nmin}{
Number of times the "minimum value" is multiplied to set threshold value. See details below.
}
  \item{mtry}{
Number of variables randomly sampled as candidates at each split. Standard \code{randomForest} parameter.
}
}
\details{
First, \code{nfor.thres} random forests are computed using the function \code{randomForest} with arguments \code{importance=TRUE}. Then variables are sorted according to their mean variable importance (VI), in decreasing order. This order is kept all along the procedure. Next, a threshold is computed: \code{min.thres}, the minimum predicted value of a pruned CART tree fitted to the curve of the standard deviations of VI. Finally, the actual thresholding is performed: only variables with a mean VI larger than \code{nmin} * \code{min.thres} are kept.
}
\value{
An object of class \code{VSURF.thres}, which is a list with the following components:
 \item{varselect.thres}{
 A vector of indices of selected variables, sorted according to their mean VI, in decreasing order.
 }
 \item{imp.varselect.thres}{
 A vector of importances of the \code{varselect.thres} variables.
 }
 \item{min.thres}{
 The minimum predicted value of a pruned CART tree fitted to the curve of the standard deviations of VI.
 }
 \item{ord.imp}{
 A list containing the order of all variables mean importance. \code{$x} contains the mean importances in decreasing order. \code{$ix} contains indices of the variables.
 }
 \item{ord.sd}{
 A vector of standard deviations of all variables importances. The order is given by \code{ord.imp}. 
 }
 \item{mean.perf}{
 The mean OOB error rate, obtained by a random forests build with all variables.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
