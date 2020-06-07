\name{ClusterCreateClassification}
\alias{ClusterCreateClassification}

\title{
Create Classification for Cluster.. functions
}
\description{
Creates a Cls from arbitrary list of objects
}
\usage{
ClusterCreateClassification(Objects)
}

\arguments{
  \item{Objects}{
Listed objects, for example factor
}
}
\details{
See example
}
\value{
 LIST, with
\item{Cls }{[1:n] numerical vector with n numbers defining the labels of the classification. It has 1 to k unique numbers representing the arbitrary labels of the classification.}
\item{ClusterNames }{ClusterNames defined which names belongs to which unique number}

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

\keyword{ClusterCreateClassification}
\keyword{factor}
\concept{Create Cluster Classification}