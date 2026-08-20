\name{ModelBasedVarSelClustering}
\alias{ModelBasedVarSelClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model-Based Clustering with Variable Selection
}
\description{
Model-based clustering with variable selection and estimation of the number of
clusters, based either on [Marbac/Sedki, 2017] and [Marbac et al., 2020] or on
[Scrucca and Raftery, 2014].
}
\usage{
ModelBasedVarSelClustering(Data,ClusterNo,Type,PlotIt=FALSE, ...)
}
\arguments{
\item{Data}{A [1:n,1:d] matrix containing the dataset to be clustered. It
consists of n cases of d-dimensional data points. Every case has d attributes,
variables, or features.}
\item{ClusterNo}{Numeric scalar defining the number of clusters to search for.}
\item{Type}{Character scalar, either \code{"VarSelLCM"} [Marbac/Sedki, 2017;
Marbac et al., 2020] or \code{"clustvarsel"} [Scrucca and Raftery, 2014].}

\item{PlotIt}{Optional logical scalar. If \code{TRUE}, the clustering is
plotted. The default is \code{FALSE}.}
\item{\dots}{Further arguments passed to \link[VarSelLCM]{VarSelCluster} or
\link[clustvarsel]{clustvarsel}.}
}
\value{
A list with the following components:
\item{Cls}{A numerical vector of length n defining the classification. It has k
unique values representing the arbitrary cluster labels.}
\item{Object}{The object returned by the selected clustering algorithm.}
}
\references{
[Marbac/Sedki, 2017] Marbac, M. and Sedki, M.: Variable selection for
model-based clustering using the integrated complete-data likelihood. Statistics
and Computing, 27(4), pp. 1049-1063, 2017.

[Marbac et al., 2020] Marbac, M., Sedki, M., and Patin, T.: Variable selection
for mixed data clustering: application in human population genomics. Journal of
Classification, Vol. 37(1), pp. 124-142, 2020.
}
\author{
Quirin Stier, Michael Thrun
}
\examples{
\dontshow{
set.seed(1)
Data <- rbind(
  matrix(rnorm(20, mean = -2), ncol = 2),
  matrix(rnorm(20, mean = 2), ncol = 2)
)
V <- ModelBasedVarSelClustering(
  Data,
  ClusterNo = 2,
  Type = "VarSelLCM"
)
Cls <- V$Cls
}

\donttest{
# Hepta
data("Hepta")
Data <- Hepta$Data
V <- ModelBasedVarSelClustering(
  Data,
  ClusterNo = 7,
  Type = "VarSelLCM"
)
Cls <- V$Cls
ClusterAccuracy(Hepta$Cls, Cls, K = 7)

V <- ModelBasedVarSelClustering(
  Data,
  ClusterNo = 7,
  Type = "clustvarsel"
)
Cls <- V$Cls
ClusterAccuracy(Hepta$Cls, Cls, K = 7)

# Hearts
heart <- VarSelLCM::heart
ztrue <- heart[, "Class"]
Data <- heart[, -13]
V <- ModelBasedVarSelClustering(Data, 2, Type = "VarSelLCM")
Cls <- V$Cls
ClusterAccuracy(ztrue, Cls, K = 2)
}
}
\concept{Variable Selection}
\concept{Model-based clustering}
