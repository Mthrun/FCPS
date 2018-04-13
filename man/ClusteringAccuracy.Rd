\name{ClusteringAccuracy}
\alias{ClusteringAccuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ClusteringAccuracy
}
\description{
ClusteringAccuracy
}
\usage{
ClusteringAccuracy(PriorCls,CurrentCls,K=9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PriorCls}{
%ground truth
}
  \item{CurrentCls}{
clustering result
}
  \item{K}{
Maximal number of classes for computation.
}
}
\details{
Here, accuracy is defined as the normalized sum over all true positive labeled data points of a clustering algorithm. The best of all permutation of labels with the highest accuracy is selected in every trial because algorithms arbitrarily define the labels.
}
\value{
Accuracy Between zero and one
}
\references{
Michael C. Thrun, Felix Pape, Alfred Ultsch: Benchmarking Cluster Analysis Methods in the Case of Distance and Density-based Structures Defined by a Prior Classification Using PDE-Optimized Violin Plots, ECDA, Potsdam, 2018
}
\author{
Michael Thrun
}

\examples{
##following
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Accuracy}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line
