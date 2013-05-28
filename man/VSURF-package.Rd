\name{VSURF-package}
\alias{VSURF-package}
\docType{package}
\title{
Variable Selection Using Random Forests
}
\description{
Three steps variable selection procedure based on random forests.
  Initially developped to handle high dimensional data (for which number
	of variables largely exceeds number of observations), the package is 
	very versatile and can treat most dimensions of data, for regression and
	supervised classification problems.
	First step is dedicated to eliminate irrelevant variables from the dataset.
	Second step aims to select all variables related to the response for
	interpretation prupose.
	Third step refines the selection by eliminating redundancy in the set of
	variables selected by the second step, for prediction prupose.
}
\details{
\tabular{ll}{
Package: \tab VSURF\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-19\cr
License: \tab GPL (>= 2)\cr
}
The most important function is the function \code{VSURF}.
}
\author{
Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot

Maintainer: <Robin.Genuer@isped.u-bordeaux2.fr>
}
\references{
Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010), \emph{Variable selection using random forests}, Pattern Recognition Letters 31(14), 2225-2236
}
%\keyword{ package }
\seealso{
 \code{\link{VSURF}}
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
