\name{RobustTrimmedClustering}
\alias{RobustTrimmedClustering}
\title{Robust Trimmed Clustering}
\description{
Robust Trimmed Clustering invented by [Garcia-Escudero et al., 2008] and implemented by [Fritz et al., 2012].
}
\usage{
RobustTrimmedClustering(Data, ClusterNo,

Alpha=0.05,PlotIt=FALSE,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix containing the dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{Alpha}{
If \code{alpha = 0}, no trimming is performed. Otherwise, this is the proportion of data points to trim. \code{\link[tclust]{tclust}} uses 0.05 by default.
}
\item{\dots}{Further arguments passed to \code{\link[tclust]{tclust}}, such as \code{nstart} (number of random initializations), \code{iter.max} (maximum number of concentration steps), \code{restr}, and \code{restr.fact}.}
}

\details{
The algorithm initializes \eqn{k} clusters randomly and performs concentration steps to improve the current cluster assignment. The maximum number of concentration steps is controlled by \code{iter.max}. The procedure is initialized \code{nstart} times and continues until convergence or until \code{iter.max} is reached. More complex datasets may require larger values of \code{nstart} and \code{iter.max}, which increases computation time. Larger values of \code{restr.fact} allow greater heterogeneity among cluster scatter matrices, whereas values near 1 impose similar scatter. The constraint type \code{restr} can be set to \code{"eigen"}, \code{"deter"}, or \code{"sigma"}. See \code{\link[tclust]{tclust}} and [Fritz et al., 2012] for details.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

\examples{
\dontshow{
set.seed(1)
Data <- rbind(
  matrix(rnorm(20, mean = -2), ncol = 2),
  matrix(rnorm(20, mean = 2), ncol = 2)
)
out <- RobustTrimmedClustering(
  Data,
  ClusterNo = 2,
  Alpha = 0,
  PlotIt = FALSE,
  nstart = 1,
  iter.max = 5
)
}

\donttest{
data("Hepta")
out <- RobustTrimmedClustering(
  Hepta$Data,
  ClusterNo = 7,
  Alpha = 0,
  PlotIt = FALSE
)
}
}
\author{Michael Thrun}

\references{
[Garcia-Escudero et al., 2008]  Garcia-Escudero, L. A., Gordaliza, A., Matran, C., & Mayo-Iscar, A.: A general trimming approach to robust cluster analysis, The annals of Statistics, Vol. 36(3), pp. 1324-1345. 2008.

[Fritz et al., 2012]  Fritz, H., Garcia-Escudero, L. A., & Mayo-Iscar, A.: tclust: An R package for a trimming approach to cluster analysis, Journal of statistical Software, Vol. 47(12), pp. 1-26. 2012.
}
\keyword{RobustTrimmedClustering}
\concept{Robust Trimmed Clustering}