\name{IsDissimilarity}
\alias{IsDissimilarity}
\title{Check whether an object represents a dissimilarity}
\description{
Tests whether an object can reasonably be interpreted as a dissimilarity or
distance  representation.

Objects inheriting from class \code{"dist"} are accepted directly. Numeric
square matrices and data frames are checked for finiteness, approximate
symmetry, an approximately zero diagonal, and non-negative entries.
}
\usage{
IsDissimilarity(x)
}
\arguments{
  \item{x}{
    Object to test. Typically a \code{stats::dist} object, numeric matrix,
    data frame, or another R object.
  }
}
\value{
Logical scalar.

\code{TRUE} if \code{x} is recognized as a distance representation;
otherwise \code{FALSE}.
}
\details{
An object inheriting from class \code{"dist"} is returned as \code{TRUE}
without further matrix validation.

For matrix or data-frame input, \code{x} is first converted with
\code{as.matrix()}. It must then satisfy all of the following requirements:

\itemize{
  \item it is numeric;
  \item it is two-dimensional;
  \item it is square;
  \item it contains at least two observations;
  \item it contains no missing values;
  \item all entries are finite.
}

More reliable than calling isSymmetric() directly on every possible input. 
Within floating-point tolerance the check requires a zero diagonal, symmetric and nonnegative entries. The check
also reduces accidental classification of a square, symmetric data matrix as a distance matrix.

The floating-point tolerance is calculated as

\deqn{
100 \epsilon \max(1, \max |x|),
}

For a distance, the triangle inequality must additionally be satisfied, which is not checked here.
}
\note{
Small negative values caused by floating-point round-off are tolerated, but
the function does not modify the supplied object.
}
\examples{
X <- matrix(
  c(
    0, 1, 2,
    1, 0, 3,
    2, 3, 0
  ),
  nrow = 3,
  byrow = TRUE
)

IsDissimilarity(X)
# TRUE

IsDissimilarity(stats::as.dist(X))
# TRUE

IsDissimilarity(matrix(1:6, nrow = 2))
# FALSE

bad <- X
bad[1, 2] <- 10
IsDissimilarity(bad)
# FALSE
}
\seealso{
\code{\link[stats]{dist}},
\code{\link[base]{isSymmetric}}
}
\keyword{distance}
\keyword{IsDissimilarity}
\keyword{utilities}