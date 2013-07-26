\name{VSURF.thres.tune}

\alias{VSURF.thres.tune}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Tuning of the thresholding step of VSURF
}

\description{
This function allows to tune the "thresholding step" of VSURF, without
rerunning all computations.
}

\usage{
VSURF.thres.tune(res.thres, nmin = 1)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res.thres}{
    An object of class \code{VSURF.thres}, which is the result of the
    \code{\link{VSURF.thres}} function.
  }
  \item{nmin}{
Number of times the "minimum value" is multiplied to set threshold value. See details below.    
}
}

\details{
  In \code{\link{VSURF.thres}} function, the actual threshold is
  performed like this: only variables with a mean VI larger
  than \code{nmin} * \code{min.thres} are kept.
  The function \code{VSURF.thres.tune} allows you to change the value of
  \code{nmin} (which multiply the estimated threshold value
  \code{min.thres}), without rerunning all computations.
  To get a softer threshold than default, choose a value of \code{nmin} less than 1,
  and to get a harder one, choose a value larger than 1.
}

\value{
A list with the following components:
 \item{varselect.thres}{
 A vector of indices of selected variables, sorted according to their mean VI, in decreasing order.
 }
 \item{imp.varselect.thres}{
 A vector of importances of the \code{varselect.thres} variables.
 }
 \item{min.thres}{
 The minimum predicted value of a pruned CART tree fitted to the curve of the standard deviations of VI.
 }
 \item{num.varselect.thres}{
   The number of selected variables.
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
 \item{pred.pruned.treee}{
   The predictions of the CART tree fitted to the curve of the standard deviations of VI.
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
\code{\link{VSURF}}, \code{\link{VSURF.thres}}
}
\examples{
\dontrun{
data(iris)
iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
iris.thres.tuned <- VSURF.thres.tune(res.thres=iris.thres, nmin=10)
iris.thres.tuned}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
