\name{ModelBasedVarSelClustering}
\alias{ModelBasedVarSelClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model Based Clustering with Variable Selection
}
\description{
Model-based clustering with variable selection and estimation of the number of
clusters which is either based on [Marbac/Sedki, 2017],[Marbac et al., 2020], or on [Scrucca and Raftery, 2014].
}
\usage{
ModelBasedVarSelClustering(Data,ClusterNo,Type,PlotIt=FALSE, ...)
}
\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases
of d-dimensional data points. Every case has d attributes, variables or
features.}
\item{ClusterNo}{Numeric which defines number of cluster to search for.}
\item{Type}{String, either \code{VarSelLCM} [Marbac/Sedki, 2017],[Marbac et al., 2020], or \code{clustvarsel} [Scrucca and Raftery, 2014].}

\item{PlotIt}{(optional) Boolean. Default = FALSE = No plotting performed.}
\item{\dots}{Further arguments passed on to \link[VarSelLCM]{VarSelCluster} or \link[clustvarsel]{clustvarsel}.}
}
\value{
List of
\item{Cls}{[1:n]  numerical vector with n numbers defining the classification as
the main output of the clustering algorithm. It has k unique numbers
representing the arbitrary labels of the clustering.}
\item{Object}{Object defined by clustering algorithm as the other output of this
algorithm}
}
\references{
[Marbac/Sedki, 2017] Marbac, M. and Sedki, M.: Variable selection for
model-based clustering using the integrated complete-data likelihood. Statistics
and Computing, 27(4), pp. 1049-1063, 2017.

[Marbac et al., 2020]  Marbac, M., Sedki, M., & Patin, T.: Variable selection for mixed data clustering: application in human population genomics, Journal of Classification, Vol. 37(1), pp. 124-142. 2020.


}
\author{
Quirin Stier, Michael Thrun
}
\examples{
# Hepta
data("Hepta")
Data = Hepta$Data
V = ModelBasedVarSelClustering(Data, ClusterNo=7,Type="VarSelLCM")
Cls = V$Cls
ClusterAccuracy(Hepta$Cls, Cls, K = 7)

V = ModelBasedVarSelClustering(Data, ClusterNo=7,Type="clustvarsel")
Cls = V$Cls
ClusterAccuracy(Hepta$Cls, Cls, K = 7)

\dontrun{
# Hearts
heart=VarSelLCM::heart
ztrue <- heart[,"Class"]
Data <- heart[,-13]
V <- ModelBasedVarSelClustering(Data,2,Type="VarSelLCM")
Cls = V$Cls
ClusterAccuracy(ztrue, Cls, K = 2)
}
}
\concept{Variable Selection}
\concept{Model-based clustering}