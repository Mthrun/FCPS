\name{HDDClustering}
\alias{HDDClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HDD clustering is a model-based clustering method of [Bouveyron et al., 2007].
}
\description{
HDD clustering is based on the Gaussian Mixture Model and on the idea that the data lives in subspaces with a lower dimension than the dimension of the original space. It uses the EM algorithm to estimate the parameters of the model [Berge et al., 2012].
}
\usage{
HDDClustering(Data, ClusterNo, PlotIt=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{Data}{[1:n,1:d] matrix containing the dataset to be clustered. It consists of n cases
of d-dimensional data points. Every case has d attributes, variables or
features.}
\item{ClusterNo}{Optional numeric value indicating the number of clusters, or a vector from 1 to k indicating the maximum expected number of clusters.}
\item{PlotIt}{(optional) Boolean. Default = FALSE = No plotting performed.}
\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used, see \code{\link[HDclassif]{hddc}} for details.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as
the main output of the clustering algorithm. It has k unique numbers
representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output
of this algorithm}
}
\details{
HDD clustering maximizes the BIC criterion over possible numbers of clusters up to \code{ClusterNo}. By default, the most general model is used. Alternatively, \code{model = "ALL"} evaluates all available models with BIC [Berge et al., 2012]. If specific properties of \code{Data} are known beforehand, see \code{\link[HDclassif]{hddc}} for model selection.
}

\references{
[Berge et al., 2012]  L. Berge, C. Bouveyron and S. Girard, HDclassif:
an R Package for Model-Based Clustering and Discriminant Analysis of
High-Dimensional Data, Journal of Statistical Software, vol. 42 (6), pp. 1-29,
2012.

[Bouveyron et al., 2007]  Bouveyron, C. Girard, S. and Schmid, C: High-Dimensional Data Clustering, Computational Statistics and Data Analysis, vol. 52 (1), pp. 502-519, 2007.

}
\author{
Quirin Stier
}
\examples{
# Hepta
data("Hepta")
Data = Hepta$Data
#Non-default parameter model
#can be set to evaulate all possible models
V = HDDClustering(Data=Data,ClusterNo=7,model="ALL")
Cls = V$Cls
\donttest{
ClusterAccuracy(Hepta$Cls, Cls)
}
\dontrun{
library(HDclassif)
data(Crabs)
Data = Crabs[,-1]
V = HDDClustering(Data=Data,ClusterNo=4,com_dim=1)
}
}
\keyword{HDDC}
\keyword{subspace}
\concept{model-based clustering}
