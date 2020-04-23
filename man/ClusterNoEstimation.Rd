\name{ClusterNoEstimation}
\alias{ClusterNoEstimation}
\title{
Estimates Number of Clusters using up to 26 Indicators
}
\description{
Calculation of up to 26 indicators and the recommendations based on them for the number of clusters in data sets. 
For a given dataset and clusterings for this dataset, key indicators mentioned in details are calculated and based on this a recommendation regarding the number of classes is given for each indicator.

An alternative estimation of the cluster number can be done by counting the valleys of the topographic map of the generalized U-Matrix for a specfic projection method using the \pkg{ProjectionBasesdClustering} and \pkg{GeneralizedUmatrix} packages on CRAN, see [Thrun/Ultsch, 2020] for details.
}
\usage{
ClusterNoEstimation(Data, ClsMatrix = NULL, max.nc, 

index = "all", min.nc = 2,

Silent = TRUE, method = NULL, PlotIt=TRUE)
}
\arguments{
  \item{Data}{
    [1:n,1:d] matrix of dataset to be clustered. It consists of n cases or d-dimensional data points. Every case has d attributes, variables or features.
  }
  \item{ClsMatrix}{
[1:n,1:(max.nc)] Clustering of the number of classes to be checked as a matrix with one cluster per column (see also details (2) and (3)), must be specified if method = NULL
  }
  \item{max.nc}{
    highest number of classes to be checked
  }
  \item{method}{
    Cluster procedure, with which the clusterings are created (see details (4) for possible methods), must be specified if ClsMatrix = NULL
	}
  Optional:
  \item{index}{
 String or vector of strings with the indicators to be calculated (see details (1)), default = "all
  }
  \item{min.nc}{
   lowest number of classes to be checked, default = 2
  }
  \item{Silent}{
      if TRUE status messages are output, default = FALSE
  }
    \item{PlotIt}{
      if TRUE plots fanplot with proposed cluster numbers
  }
}
\details{
(1)

The following 26 indicators can be calculated:
"ball", "beale", "calinski", "ccc", "cindex", "db", "duda", 
"dunn", "frey", "friedman", "hartigan", "kl", "marriot", "mcclain", 
"pseudot2", "ptbiserial", "ratkowsky", "rubin", "scott", "sdbw", 
"sdindex", "silhouette", "ssi", "tracew", "trcovw", "xuindex".

These can be specified individually or as a vector via the parameter index.
If you enter 'all', all key figures are calculated.

(2)

The indicators kl, duda, pseudot2, beale, frey and mcclain require a clustering for max.nc+1 classes. If these key figures are to be calculated, this clustering must be specified in cls.

(3)

The indicator kl requires a clustering for min.nc-1 classes. If this key figure is to be calculated, this clustering must also be specified in cls. For the case min.nc = 2 no clustering for 1 has to be given.

(4)

The following methods can be used to create clusterings:

"ward.D", "single", "complete", "average", "mcquitty" 
"median," "centroid," "ward.D2," "kmeans," "DBSclustering,"

(5)

The indicators duda, pseudot2, beale and frey are only intended for use in hierarchical cluster procedures.

}
\value{
\item{Indicators}{
  a table of the calculated indicators except Duda, Pseudot2 and Beale
}
\item{ClusterNo}{
 the recommended number of clusters for each calculated indicator
}
\item{ClsMatrix}{
  [1:n,min.nc:(max.nc)] Output of the clusterings used for the calculation
}
\item{HierarchicalIndicators}{
  Either NULL or the values for the indicators Duda, Pseudot2 and Beale in case of hierarchical cluster procedures, if calculated
}
}
\references{
Charrad, Malika, et al. "Package 'NbClust', J. Stat. Soft Vol.  61, pp. 1-36, 2014.

Dimtriadou, E. "cclust: Convex Clustering Methods and Clustering Indexes." R package version 0.6-16, URL \url{https://CRAN.R-project.org/package=cclust}, 2009.

[Thrun/Ultsch, 2020]  Thrun, M. C., and Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Artificial Intelligence, in press, \url{https://doi.org/10.1016/j.artint.2020.103237}, 2020.

}
\author{
Peter Nahrgang
}
\note{
Code of "calinski", "cindex", "db", "hartigan",
"ratkowsky", "scott", "marriot", "ball", "trcovw", "tracew", "friedman",
"rubin", "ssi" of package cclust ist adapted for the purpose of this function.
}
\examples{
# Reading the iris dataset from the standard R-Package datasets
data <- as.matrix(iris[,1:4])

# Creating the clusterings for the data set
#(here with method complete) for the number of classes 2 to 8
hc <- hclust(dist(data), method = "complete")
clsm <- matrix(data = 0, nrow = dim(data)[1],  ncol = 7)
for (i in 2:8) {
  clsm[,i-1] <- cutree(hc,i)
}

# Calculation of all indicators and recommendations for the number of classes
indicatorsList=ClusterNoEstimation(Data = data, ClsMatrix = clsm, max.nc = 7)

# Alternatively, the same calculation as above can be executed with the following call
ClusterNoEstimation(Data = data, max.nc = 7, method = "complete")
#In this variant, the function clusterumbers also takes over the clustering
}
