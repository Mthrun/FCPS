\name{ClusterRedefine}
\alias{ClusterRedefine}

\title{
Redfines Clustering
}
\description{
Redfines some or all Clusters of Clustering such that the names of the numerical vectors are defined by 
}
\usage{
ClusterRedefine(Cls, NewLabels,OldLabels)
}

\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{NewLabels}{
  [1:p], p<=k labels (identifiers) of clusters to be changed with
}
  \item{OldLabels}{
  Optional,  [1:p], p<=k labels(identifiers) of clusters to be changed, default [1:k] unique cluster Ids of \code{Cls}
}

}
\details{
The same ordering of \code{NewLabels} and  \code{OldLabels} is assumend, i.e., the mapping is defined by OldLabels[i] -> NewLabels[i] with \code{i} in [1:p].   \code{NewLabels} can also be a vector for strings, for example for plotting.
}
\value{
Cls[1:n] numerical vector named after the row names of data
}

\author{
Michael Thrun
}

\examples{
data('Lsun3D')
Cls=Lsun3D$Cls
Data=Lsun3D$Data#
#prior
ClsNew=unique(Cls)+10
#Redfined Clustering
NewCls=ClusterRedefine(Cls,ClsNew)

table(Cls,NewCls)

#require(DataVisualizations)
n=length(unique(Cls))
NewCls=ClusterRedefine(Cls,LETTERS[1:n])
#DataVisualizations package required
if(requireNamespace("DataVisualizations"))
  DataVisualizations::Classplot(Data[,1],Data[,2],
  Cls,Names=NewCls,Plotter="ggplot",Size =1.5)

}

\keyword{ClusterRename}% use one of  RShowDoc("KEYWORDS")
