\name{summary}

\alias{summary.VSURF}

\title{Summary of VSURF results}

\description{
  This function display a summary of VSURF results
}

\usage{
\method{summary}{VSURF}(object, ...)
}

\arguments{
  \item{object}{
    An object of class \code{VSURF}, which is the result of the
    \code{\link{VSURF}} function.
  }
  \item{\dots}{
    Not used.
  }
}

\details{
  This function prints the total computation time of VSURF.
  It also gives the number of selected variables (and the computation
  time) at each step of VSURF.
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
\code{\link{VSURF}}, \code{\link{plot.VSURF}}
}

\examples{
\dontrun{
data(iris)
iris.vsurf <- VSURF(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20,
                    nfor.interp=10, nfor.pred=10)
summary(iris.vsurf)

# A more interesting example with toys data (see \code{\link{toys}})
# (a few minutes to execute)
data(toys)
toys.vsurf <- VSURF(x=toys$x, y=toys$y)
summary(toys.vsurf)}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
