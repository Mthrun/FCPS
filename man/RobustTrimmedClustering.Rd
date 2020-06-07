\name{RobustTrimmedClustering}
\alias{RobustTrimmedClustering}
\title{Robust Trimmed Clustering}
\description{
Robust Trimmed Clustering invented by [Garcia-Escudero et al., 2008] and implemented by [Fritz et al., 2012].
}
\usage{
RobustTrimmedClustering(Data, ClusterNo,Alpha,

PlotIt=FALSE,...)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

\item{ClusterNo}{A number k which defines k different clusters to be built by the algorithm.}
\item{PlotIt}{Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}

\item{Alpha}{
No trimming is done equals to alpha =0, otherwise proportion of datapoints to be trimmed.
}
\item{\dots}{Further arguments to be set for the clustering algorithm, e.g.  \code{restr} and \code{restr.fact} described in details. If not set, default arguments are used.}
}

\details{
"The larger \code{restr.fact} is chosen, the looser is the restriction on the scatter matrices, allowing for more heterogeneity among the clusters. On the contrary, small values of restr.fact close to 1 imply very equally scattered clusters. This idea of constraining cluster scatters to avoid spurious solutions goes back to Hathaway (1985), who proposed it in mixture fitting problems" [Fritz et al., 2012]. The type of constraint \code{restr} can be set to "eigen", "deter" or "sigma.". Please see \code{\link[tclust]{tclust}} for further parameter description.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}

\examples{
data('Hepta')
out=RobustTrimmedClustering(Hepta$Data,ClusterNo=7,Alpha=0,PlotIt=FALSE)
}
\author{Michael Thrun}

\references{
[Garcia-Escudero et al., 2008]  Garcia-Escudero, L. A., Gordaliza, A., Matran, C., & Mayo-Iscar, A.: A general trimming approach to robust cluster analysis, The annals of Statistics, Vol. 36(3), pp. 1324-1345. 2008.

[Fritz et al., 2012]  Fritz, H., Garcia-Escudero, L. A., & Mayo-Iscar, A.: tclust: An R package for a trimming approach to cluster analysis, Journal of statistical Software, Vol. 47(12), pp. 1-26. 2012.
}
\keyword{RobustTrimmedClustering}
\concept{Robust Trimmed Clustering}