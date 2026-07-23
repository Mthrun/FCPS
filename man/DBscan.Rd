\name{DBSCAN}
\alias{DBSCAN}
\alias{DBscan}

\title{
DBSCAN
}
\description{
Density-Based Spatial Clustering of Applications with Noise of [Ester et al., 1996].
}
\usage{
DBSCAN(Data,Radius,minPts,Rcpp=TRUE,

PlotIt=FALSE,UpperLimitRadius,\dots)
}

\arguments{
\item{Data}{[1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.}

  \item{Radius}{
 Eps [Ester et al., 1996, p. 227] neighborhood in the R-ball graph/unit disk graph), size of the epsilon neighborhood.
 If NULL, automatic estimation is performed using insights of [Ultsch, 2005].
}
  \item{minPts}{
 Number of minimum points in the eps region (for core points). 
 In principle minimum number of points in the unit disk, if the unit disk is within the cluster (core) [Ester et al., 1996, p. 228].
 If NULL, 2.5 percent of points is selected.
}
  \item{Rcpp}{
If TRUE: fast Rcpp implementation of mlpack is used. FALSE uses dbscan package.
}


\item{PlotIt}{Default: FALSE, If TRUE plots the first three dimensions of the dataset with colored three-dimensional data points defined by the clustering stored in \code{Cls}}
\item{UpperLimitRadius}{Limit for radius search, experimental}

\item{\dots}{Further arguments to be set for the clustering algorithm, if not set, default arguments are used.}

}
\details{

To simplify, the radius works as follows. When DBSCAN looks at a data point, it draws a circle around it with a \code{Radius}.
If there are enough data points inside that circle, they become a group. Choosing the right \code{Radius} depends on how spread out your data points are.

This is the minimum number of data points  \code{minPts} that should be inside the circle for DBSCAN to consider them a group. If there are fewer data points than this number, the data point might be considered as noise or an outlier. 
The \code{minPts}  value helps control how strict or lenient the algorithm is when forming groups.
}
\value{
List of
\item{Cls}{[1:n]  numerical vector defining the clustering; this classification is the main output of the algorithm. Points which cannot be assigned to a cluster will be reported as members of the noise cluster with 0.}
\item{Object}{Object defined by clustering algorithm as the other output of this algorithm}
}
\references{
[Ester et al., 1996]  Ester, M., Kriegel, H.-P., Sander, J., & Xu, X.: A density-based algorithm for discovering clusters in large spatial databases with noise, Proc. Kdd, Vol. 96, pp. 226-231, 1996.

[Ultsch, 2005]  Ultsch, A.: Pareto density estimation: A density estimation for knowledge discovery, In Baier, D. & Werrnecke, K. D. (Eds.), Innovations in classification, data science, and information systems, (Vol. 27, pp. 91-100), Berlin, Germany, Springer, 2005.
}
\author{
Michael Thrun
}

\examples{
data('Hepta')

out=DBSCAN(Hepta$Data,Radius=NULL,minPts=NULL,PlotIt=FALSE)

\dontrun{
#search for right parameter setting by grid search
data("WingNut")
Data = WingNut$Data
DBSGrid <- expand.grid(
  Radius = seq(from = 0.01, to = 0.3, by = 0.02),
  minPTs = seq(from = 1, to = 50, by = 2)
)
BestAcc = c()
for (i in seq_len(nrow(DBSGrid))) {
  parameters <- DBSGrid[i,]
  Cls9 = DBSCAN(
    Data,
    minPts = parameters$minPTs,
    
    Radius = parameters$Radius,
    PlotIt = F,
    
    UpperLimitRadius = parameters$Radius
  )$Cls
  if (length(unique(Cls9)) < 5)
    BestAcc[i] = ClusterAccuracy(WingNut$Cls,
                                    
                                    Cls9) * 100
  else
    BestAcc[i] = 50
}
max(BestAcc)
which.max(BestAcc)
parameters <- DBSGrid[13,]

Cls9 = DBSCAN(
  Data,
  minPts = parameters$minPTs,
  Radius = parameters$Radius,
  UpperLimitRadius = parameters$Radius, 
  PlotIt = TRUE
)$Cls
}
}
\keyword{DBSCAN}% use one of  RShowDoc("KEYWORDS")
\keyword{Clustering}% __ONLY ONE__ keyword per line