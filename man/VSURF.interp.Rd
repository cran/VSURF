\name{VSURF.interp}

\alias{VSURF.interp}
\alias{VSURF.interp.default}
\alias{VSURF.interp.formula}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Interpretation step of VSURF
}

\description{
  Interpretation step aims to select all variables related to the response for interpretation prupose. This is the second step of the \code{\link{VSURF}} function. It is designed to be executed after the thresholding step \code{\link{VSURF.thres}}.
}

\usage{
\method{VSURF.interp}{formula}(formula, data, ..., na.action=na.fail)
\method{VSURF.interp}{default}(x, y, vars, nfor.interp = 25, nsd = 1, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a data frame containing the variables in the model.
  }
  \item{na.action}{A function to specify the action to be taken if NAs
    are found.  (NOTE: If given, this argument must be named, and as
    \code{randomForest} it is only used with the formula-type call.)}
  \item{x, formula}{
    A data frame or a matrix of predictors, the columns represent the
    variables. Or a formula describing the model to be fitted.
  }
  \item{y}{
    A response vector (must be a factor for classification problems and
    numeric for regression ones).
  }
  \item{vars}{
    A vector of variable indices. Typically, indices of variables selected by thresholding step (see value \code{varselect.thres} of \code{\link{VSURF.thres}} function).
  }
  \item{nfor.interp}{
    Number of forests grown.
  }
  \item{nsd}{
    Number of times the standard deviation of the minimum value of \code{err.interp} is multiplied. See details below.
  }
  \item{...}{
    others parameters to be passed on to the \code{randomForest}
    function (see ?randomForest for further information)
  }
}

\details{
  \code{nfor.interp} embedded random forests models are grown,
  starting with the random forest build with only the most important
  variable and ending with all variables.
  Then, \code{err.min} the minimum mean out-of-bag (OOB) error rate
  of these models and its associated standard deviation \code{sd.min}
  are computed.
  Finally, the smallest model (and hence its corresponding variables)
  having a mean OOB error less than \code{err.min} + \code{nsd} * \code{sd.min} is selected.
}

\value{
  An object of class \code{VSURF.interp}, which is a list with the following components:
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
    \item{comput.time}{
    Computation time.
  }
  \item{call}{
      The original call to \code{VSURF.interp}.
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
  \code{\link{VSURF}}, \code{\link{tune}}
}

\examples{
data(iris)
iris.thres <- VSURF.thres(x=iris[,1:4], y=iris[,5], ntree=100, nfor.thres=20)
iris.interp <- VSURF.interp(x=iris[,1:4], y=iris[,5], vars=iris.thres$varselect.thres,
                            nfor.interp=10)
iris.interp

\dontrun{
# A more interesting example with toys data (see \code{\link{toys}})
# (a few minutes to execute)
data(toys)
toys.thres <- VSURF.thres(x=toys$x, y=toys$y)
toys.interp <- VSURF.interp(x=toys$x, y=toys$y, vars=toys.thres$varselect.thres)
toys.interp}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
