\name{PenalizedRegressionBasedClustering}
\alias{PenalizedRegressionBasedClustering}

\title{
Penalized Regression-Based Clustering of [Wu et al., 2016].
}
\description{
Clustering is performed through penalized regression with grouping pursuit
}
\usage{
PenalizedRegressionBasedClustering(Data, FirstLambda, 

SecondLambda, Tau, PlotIt = FALSE, \dots)
}

\arguments{
  \item{Data}{
[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
}
  \item{FirstLambda}{
 Set 1 for quadratic penalty based algorithm, 0.4 for revised ADMM.
}
  \item{SecondLambda}{
The magnitude of grouping penalty.
}
  \item{Tau}{
Tuning parameter: tau, related to grouping penalty.
}
  \item{PlotIt}{
Default: FALSE, if TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}
}
  \item{\dots}{
Further arguments for \code{\link[prclust]{PRclust}}, enables also usage of [Pan et al., 2013].
}
}
\details{
Parameters are rather challenging to choose.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as the main output of the clustering algorithm. It has k unique numbers representing the arbitrary labels of the clustering.
}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Pan et al., 2013]  Pan, W., Shen, X., & Liu, B.: Cluster analysis: unsupervised learning via supervised learning with a non-convex penalty, The Journal of Machine Learning Research, Vol. 14(1), pp. 1865-1889. 2013.

[Wu et al., 2016]  Wu, C., Kwon, S., Shen, X., & Pan, W.: A new algorithm and theory for penalized regression-based clustering, The Journal of Machine Learning Research, Vol. 17(1), pp. 6479-6503. 2016.


}
\author{
Michael Thrun
}
\note{
Data matrix is internally transposed in order to fit the definition of the algorithm.
}

\examples{
data(Hepta)
Data=Hepta$Data
out=PenalizedRegressionBasedClustering(Data,0.4,1,2,FALSE)
table(out$Cls,Hepta$Cls)
}
\keyword{ADMM}
\keyword{PenalizedRegressionBasedClustering}
\concept{DC-ADMM}
\concept{Penalized Regression Based Clustering}