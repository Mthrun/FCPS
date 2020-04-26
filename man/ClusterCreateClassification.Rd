\name{ClusterCreateClassification}
\alias{ClusterCreateClassification}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Classification for Cluster.. functions
}
\description{
Creates a Cls from arbitrary list of objects
}
\usage{
ClusterCreateClassification(Objects)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Objects}{
listed objects, for example factor
}
}
\details{
See example
}
\value{
 LIST, with
\item{Cls }{[1:n] numerical vector with n numbers defining the labels of the classification. It has 1 to k unique numbers representing the arbitrary labels of the classification.}
\item{ClusterNames }{ClusterNames definined which names belongs to which unique number}

}

\author{
Michael Thrun
}

\examples{

\dontrun{
  Iris=datasets::iris
  SomeFactors=Iris$Species
  V=ClusterCreateClassification(SomeFactors)
  Cls=V$Cls
  V$ClusterNames
  table(Cls,SomeFactors)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ClusterCreateClassification}% use one of  RShowDoc("KEYWORDS")
\keyword{factor}% __ONLY ONE__ keyword per line
