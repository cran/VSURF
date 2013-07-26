\name{VSURF.interp.tune}

\alias{VSURF.interp.tune}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
Tuning of the interpretation step of VSURF
}

\description{
This function allows to tune the "interpretation step" of VSURF, without
rerunning all computations.
}

\usage{
VSURF.interp.tune(res.interp, nsd = 1)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res.interp}{
    An object of class \code{VSURF.interp}, which is the result of the
    \code{VSURF.interp} function.    
  }
  \item{nsd}{
Number of times the standard deviation of the minimum value of \code{err.interp} is multiplied. See details below.
}
}

\details{
  In \code{\link{VSURF.interp}} function, the smallest model
  (and hence its corresponding variables) having a mean OOB error rate
  less than \code{err.min} + \code{nsd} * \code{sd.min} is selected.
  The function \code{VSURF.interp.tune} allows to change the value of
  \code{nsd} (which multiply the standard deviation of the minimum OOB
  error rate \code{sd.min}), without rerunning all computations.
  To get a larger model than default, choose a value of \code{nsd} less than 1,
  and to get a smaller one, choose a value larger than 1.
}

\value{
A list with the following components:
 \item{varselect.interp}{
 A vector of indices of selected variables.
 }
 \item{err.interp}{
 A vector of the mean OOB error rates of the embedded random forests models.
 }
 \item{sd.min}{
 The standard deviation of OOB error rates associated to the random forests model attaining the minimum mean OOB error rate.
 }
 \item{num.varselect.interp}{
   The number of selected variables.
 }
 \item{varselect.thres}{
 A vector of indexes of variables selected after "thresholding step", sorted according to their mean VI, in decreasing order.
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
\code{\link{VSURF}}, \code{\link{VSURF.interp}}
}

\examples{
\dontrun{
data(iris)
iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
iris.interp <- VSURF.interp(x=iris[,1:4], y=iris[,5], vars=iris.thres$varselect.thres,
                            nfor.interp=10)
iris.interp
iris.interp.tuned <- VSURF.interp.tune(res.interp=iris.interp, nsd=10)
iris.interp.tuned}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
