\name{ClusterCreateClassification}
\alias{ClusterCreateClassification}

\title{
Create Classification for Cluster.. functions
}
\description{
Creates a Cls from arbitrary list of objects
}
\usage{
ClusterCreateClassification(Objects,Decreasing,na.rm=FALSE)
}

\arguments{
  \item{Objects}{
Listed objects, for example factor
}
  \item{Decreasing}{
Boolean that can be missing. If given, sorts \code{ClusterNames} with either decreasing or increasing.
}
  \item{na.rm}{
Boolean, if TRUE all non finite values of Cls are set to 0. FALSE: NaN and Inf and -Inf are set to different numeric labels, NA is set to NaN 
}
}
\details{
\code{ClusterNames} can be sorted before the classification stored \code{Cls} is created. See example.
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
  
  #Increasing alphabetical order
  V=ClusterCreateClassification(SomeFactors,Decreasing=FALSE)
  Cls=V$Cls
  V$ClusterNames
  table(Cls,SomeFactors)
  }
}

\keyword{ClusterCreateClassification}
\keyword{factor}
\concept{Create Cluster Classification}