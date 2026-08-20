\name{ClusterRedefine}
\alias{ClusterRedefine}

\title{
Redefines Clustering
}
\description{
Redefines some or all labels in a clustering according to a supplied mapping.
}
\usage{
ClusterRedefine(Cls, NewLabels,OldLabels,Silent=FALSE)
}

\arguments{
  \item{Cls}{
1:n numerical vector of numbers defining the classification as the main output of the clustering algorithm for the n cases of data. It has k unique numbers representing the arbitrary labels of the clustering.
}
  \item{NewLabels}{
  [1:p], p<=k labels (identifiers) of clusters to be changed with
}
  \item{OldLabels}{
  Optional vector of p labels, with p <= k, identifying the clusters to change. The default is all unique cluster labels in \code{Cls}.
}
  \item{Silent}{
  Optional logical scalar. If \code{TRUE}, warning messages are suppressed.
}
}
\details{
The same ordering of \code{NewLabels} and \code{OldLabels} is assumed; that is, the mapping is \code{OldLabels[i] -> NewLabels[i]} for \code{i} in [1:p]. \code{NewLabels} may also be a character vector, for example for plotting.
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
