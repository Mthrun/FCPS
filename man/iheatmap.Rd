\name{iheatmap}
\alias{iheatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HeatMap with dendrogramms
}
\description{
Heatmap with complete linakge algorithm on euclidean distances:
This heatmaps adds to the rows and to lines dendrogramms based \code{\link{hclust}} function.
}
\usage{
iheatmap(Data = NULL, FestgesetzteClustAnz = 0, Directory = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
Der Datensatz, bein NaN werden diese automatisch zu Mittelwerten umgewandelt
}
  \item{FestgesetzteClustAnz}{
in soviele Cluster werden die daten eingeteilt, wenn dieser Wert 
fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet
}
  \item{Directory}{
directory where *.lrn file is  (default ==  getwd() )
}
}
\details{

A heat map is a false color image (basically image(t(x))) with a dendrogram added to the left side and/or to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out.

This heatmap provides a number of extensions to the standard R heatmap function.

}
\value{
Liste V with Cls (if FestgesetzteClustAnz!=0) and Results of heatmap.2
}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{heatmap.2}}
}

\keyword{heatmap}
