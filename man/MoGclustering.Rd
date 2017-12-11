\name{MoGclustering}
\alias{MoGclustering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MoGclustering
}
\description{
call R's Model based clustering or MixtureOfGaussians (MoG) clustering
}
\usage{
MoGclustering(Data,FestgesetzteClustAnz=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{ [1:n,1:d] der Datensatz in Zeilenvektoren }
	\item{FestgesetzteClustAnz}{the number of clusters, say k}

}
\details{
see [Thrun, 2017, p. 23] or [Fraley/Raftery, 2002] and [Fraley/Raftery, 2006].
}
\value{
  \item{Cls}{[1:n], Clusterung der Daten}
}
\references{
[Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2017.

[Fraley/Raftery, 2002]  Fraley, C., and Raftery, A. E.: Model-based clustering, discriminant analysis, and density estimation, Journal of the American Statistical Association, Vol. 97(458), pp. 611-631. 2002.

[Fraley/Raftery, 2006]  Fraley, C., and Raftery, A. E.MCLUST version 3: an R package for normal mixture modeling and model-based clustering,DTIC Document, 2006.
}
\author{
Michael Thrun
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MixtureOfGaussians}% use one of  RShowDoc("KEYWORDS")
\keyword{MoG}% __ONLY ONE__ keyword per line
\keyword{Model based clustering}% __ONLY ONE__ keyword per line