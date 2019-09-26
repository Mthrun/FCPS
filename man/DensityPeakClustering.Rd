\name{DensityPeakClustering}
\alias{DensityPeakClustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density Peak Clustering algorithm using the Decision Graph
}
\description{
Density Peaks Clustering of [Rodriguez/Laio, 2014] is here implemented by [Pedersen et al., 2017] with estimation of [Wang et al, 2015] meaning its non adaptive in the sense of \code{\link{ADPclustering}}.
}
\usage{
DensityPeakClustering(DataOrDistances, Rho,Delta,Dc,Knn=7, 

method = "euclidean", PlotIt = FALSE, Data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataOrDistances}{
Either [1:n,1:n] symmetric distance matrix or [1:n,1:d] not symmetric data matrix of n cased and d variable
}

  \item{Rho}{
local density of a point, See [Rodriguez/Laio, 2014] for explanation
}
  \item{Delta}{
minimum distance between a point and any other point, See [Rodriguez/Laio, 2014] for explanation
}
  \item{Dc}{
Optional, Cutoff distance, will either be estimated by [Pedersen et al., 2017] or  [Wang et al, 2015] (see example below)
}
  \item{Knn}{
Optional k nearest neighbors
}

  \item{method}{
Optional distance method of data, default is euclid, see \code{\link[parallelDist]{parDist}} for details
}
  \item{PlotIt}{
Optional TRUE: Plots 2d or 3d result with clustering
}
\item{Data}{[1:n,1:d] data matrix in the case that \code{DataOrDistances} is missing and partial matching does not work.}

  \item{\dots}{
Optional, further arguments for  \code{\link[densityClust]{densityClust}}
}
}
\details{
The densityClust algorithm does not decide the k number of clusters, this has to be done by the parameter setting. This contrary to the other version of the algorithm of another package which can be called with \code{\link{ADPclustering}}.

The plot shows the density peaks (Cluster centers). Set Rho and Delta as boundaries below the number of relevant cluster centers for your problem. (see example below).
}
\value{
If Rho and Delta are set:

list of

Cls : Clustering as a numeric vector of k clusters

DensityPeaks: output of [Pedersen et al., 2017] algorithm

If Rho and Delta are missing:

\code{\link[plotly]{plot_ly}} object of the decision graph is returned
}
\references{
[Wang et al., 2015] Wang, S., Wang, D., Li, C., & Li, Y.: Comment on" Clustering by fast search and find of density peaks", arXiv preprint arXiv:1501.04267, 2015.

[Pedersen et al., 2017]  Thomas Lin Pedersen, Sean Hughes and Xiaojie Qiu: densityClust: Clustering by Fast Search and Find of Density Peaks. R package version 0.3. https://CRAN.R-project.org/package=densityClust, 2017.

[Rodriguez/Laio, 2014]  Rodriguez, A., & Laio, A.: Clustering by fast search and find of density peaks, Science, Vol. 344(6191), pp. 1492-1496. 2014.


}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ADPclustering}}

\code{\link[densityClust]{densityClust}}
}
\examples{
data(Hepta)
H=EntropyOfDataField(Hepta$Data,  seq(from=0,to=1.5,by=0.05),PlotIt=TRUE)
Sigmamin=names(H)[which.min(H)]
Dc=3/sqrt(2)*as.numeric(names(H)[which.min(H)])
#look at the plot and estimate rho and delta
DensityPeakClustering(Hepta$Data, Knn = 7,Dc=Dc)
Cls=DensityPeakClustering(Hepta$Data,Dc=Dc,Rho = 0.028,

Delta = 22,Knn = 7,PlotIt = TRUE)$Cls

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Density Peak Clustering}% use one of  RShowDoc("KEYWORDS")
\keyword{Density Peak}% __ONLY ONE__ keyword per line
