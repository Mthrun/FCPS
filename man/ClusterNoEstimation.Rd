\name{ClusterNoEstimation}
\alias{ClusterNoEstimation}
\title{
Estimates Number of Clusters using up to 26 Indicators
}
\description{
Calculation of up to 26 indicators and the recommendations based on them for the number of clusters in data sets. 
For a given dataset and clusterings for this dataset, key indicators mentioned in details are calculated and based on this a recommendation regarding the number of clusters is given for each indicator.

An alternative estimation of the cluster number can be done by counting the valleys of the topographic map of the generalized U-Matrix for a specfic projection method using the \pkg{ProjectionBasesdClustering} and \pkg{GeneralizedUmatrix} packages on CRAN, see [Thrun/Ultsch, 2021] for details.
}
\usage{
ClusterNoEstimation(DataOrDistances, ClsMatrix = NULL, MaxClusterNo, 

ClusterIndex = "all", Method = NULL, MinClusterNo = 2,

Silent = TRUE,PlotIt=FALSE,SelectByABC=TRUE,Colorsequence,\dots)
}
\arguments{
  \item{DataOrDistances}{
    Either [1:n,1:d] matrix of dataset to be clustered. It consists of n cases of d-dimensional data points. Every case has d attributes, variables or features.
    
    or

Symmetric [1:n,1:n] distance matrix
  }
  \item{ClsMatrix}{
[1:n,1:(MaxClusterNo)] matrix of clusterings each columns is defined as: 

1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.

(see also details (2) and (3)), must be specified if method = NULL
  }
  \item{MaxClusterNo}{
    Highest number of clusters to be checked
  }
  \item{Method}{
    Cluster procedure, with which the clusterings are created (see details (4) for possible methods), must be specified if ClsMatrix = NULL
	}
  Optional:
  \item{ClusterIndex}{
 String or vector of strings with the indicators to be calculated (see details (1)), default = "all
  }
  \item{MinClusterNo}{
   Lowest number of clusters to be checked, default = 2
  }
  \item{Silent}{
      If TRUE status messages are output, default = FALSE
  }
    \item{PlotIt}{
      If TRUE plots fanplot with proposed cluster numbers
  }
      \item{SelectByABC}{
      If PlotIt=TRUE, TRUE: Plots group A of ABCanalysis of the most important ones (highest overlap in indicators), FALSE: plots all indicators
  }
        \item{Colorsequence}{
    Optional, character vector of sufficient length of colors for the fan plot.If the sequence is too long the first part of the sequence is used.
  }
          \item{\dots}{
    Optional, further arguents used if clustering methods if \code{Method} is set.
  }
}
\details{
Each column of \code{ClsMatrix} has to have at least two unqiue clusters defined. Otherwise the function will stop.

(1)

The following 26 indicators can be calculated:
"ball", "beale", "calinski", "ccc", "cindex", "db", "duda", 
"dunn", "frey", "friedman", "hartigan", "kl", "marriot", "mcclain", 
"pseudot2", "ptbiserial", "ratkowsky", "rubin", "scott", "sdbw", 
"sdindex", "silhouette", "ssi", "tracew", "trcovw", "xuindex".

These can be specified individually or as a vector via the parameter index.
If you enter 'all', all key figures are calculated.

(2)

The indicators kl, duda, pseudot2, beale, frey and mcclain require a clustering for MaxClusterNo+1 clusters. If these key figures are to be calculated, this clustering must be specified in cls.

(3)

The indicator kl requires a clustering for MinClusterNo-1 clusters. If this key figure is to be calculated, this clustering must also be specified in cls. For the case MinClusterNo = 2 no clustering for 1 has to be given.

(4)

The following methods can be used to create clusterings:

"kmeans," "DBSclustering","DivisiveAnalysisClustering","FannyClustering", "ModelBasedClustering","SpectralClustering" or all methods found in \code{\link{HierarchicalClustering}}.

(5)

The indicators duda, pseudot2, beale and frey are only intended for use in hierarchical cluster procedures.

If a distances matrix is given, then \pkg{ProjectionBasedClustering} is required to be accessible.
}
\value{
\item{Indicators}{
  A table of the calculated indicators except Duda, Pseudot2 and Beale
}
\item{ClusterNo}{
 The recommended number of clusters for each calculated indicator
}
\item{ClsMatrix}{
  [1:n,MinClusterNo:(MaxClusterNo)] Output of the clusterings used for the calculation
}
\item{HierarchicalIndicators}{
  Either NULL or the values for the indicators Duda, Pseudot2 and Beale in case of hierarchical cluster procedures, if calculated
}
}
\references{
Charrad, Malika, et al. "Package 'NbClust', J. Stat. Soft Vol.  61, pp. 1-36, 2014.

Dimtriadou, E. "cclust: Convex Clustering Methods and Clustering Indexes." R package version 0.6-16, URL \url{https://CRAN.R-project.org/package=cclust}, 2009.

[Thrun/Ultsch, 2021]  Thrun, M. C., and Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Artificial Intelligence, Vol. 290, pp. 103237, \doi{10.1016/j.artint.2020.103237}, 2021.

}
\author{
Peter Nahrgang, revised by Michael Thrun (2021)
}
\note{
Code of "calinski", "cindex", "db", "hartigan",
"ratkowsky", "scott", "marriot", "ball", "trcovw", "tracew", "friedman",
"rubin", "ssi" of package cclust ist adapted for the purpose of this function.

Colorsequence works if \pkg{DataVisualizations} 1.1.13 is installed (currently only on github available).
}
\examples{
# Reading the iris dataset from the standard R-Package datasets
data <- as.matrix(iris[,1:4])
MaxClusterNo = 7
# Creating the clusterings for the data set
#(here with method complete) for the number of clusters 2 to 8
hc <- hclust(dist(data), method = "complete")
clsm <- matrix(data = 0, nrow = dim(data)[1],

ncol = MaxClusterNo)
for (i in 2:(MaxClusterNo+1)) {
  clsm[,i-1] <- cutree(hc,i)
}

# Calculation of all indicators and recommendations for the number of clusters
indicatorsList=ClusterNoEstimation(Data = data, 

ClsMatrix = clsm, MaxClusterNo = MaxClusterNo)

# Alternatively, the same calculation as above can be executed with the following call
ClusterNoEstimation(Data = data, MaxClusterNo = 7, Method = "CompleteL")
# In this variant, the function clusterumbers also takes over the clustering
}
\concept{Estimation of Number of Clusters}
\keyword{ClusterNoEstimation}