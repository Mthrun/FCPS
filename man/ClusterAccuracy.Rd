\name{ClusteringAccuracy}
\alias{ClusterAccuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ClusterAccuracy
}
\description{
ClusterAccuracy
}
\usage{
ClusterAccuracy(PriorCls,CurrentCls,K=9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{PriorCls}{
Ground truth,[1:n] numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the clustering.
}
\item{CurrentCls}{
Main output of the clustering, [1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the arbitrary labels of the clustering.
}
\item{K}{
Maximal number of classes for computation.
}
}
\details{
Here, accuracy is defined as the normalized sum over all true positive labeled data points of a clustering algorithm. The best of all permutation of labels with the highest accuracy is selected in every trial because algorithms arbitrarily define the labels [Thrun et al., 2018].

In contrast to the F-measure, "Accuracy  tends  to  be  naturally  unbiased,   because  it can  be  expressed  in  terms  of a  binomial  distribution: A success in  the  underlying  Bernoulli  trial  would  be defined as sampling an example for which a classifier under consideration  makes  the  right  prediction. By definition, the success probability is  identical  to  the  accuracy  of  the classifier.  The i.i.d. assumption implies that each example of  the  test  set  is  sampled  independently,  so  the  expected fraction  of  correctly  classified  samples  is  identical  to  the probability  of  seeing  a  success  above. Averaging  over multiple  folds  is  identical  to  increasing  the  number  of repetitions of the Binomial trial.  This does not affect the posterior distribution of accuracy 
if the test sets are of equal size, or if we weight each estimate by the size of each test set." [Forman/Scholz, 2010]
}
\value{
Accuracy between zero and one
}
\references{
[Thrun et al., 2018]	Michael C. Thrun, Felix Pape, Alfred Ultsch: Benchmarking Cluster Analysis Methods in the Case of Distance and Density-based Structures Defined by a Prior Classification Using PDE-Optimized Violin Plots, ECDA, Potsdam, 2018

[Forman/Scholz, 2010]  Forman, G., and Scholz, M.: Apples-to-apples in cross-validation studies: pitfalls in classifier performance measurement, ACM SIGKDD Explorations Newsletter, Vol. 12(1), pp. 49-57. 2010.

}
\author{
Michael Thrun
}

\examples{
\dontshow{
data('Lsun3D')
Cls=Cls=kmeansClustering(Lsun3D$Data,3)
ClusterAccuracy(Lsun3D$Cls,Cls$Cls)
}
#Influence of random sets/ random starts on k-means
\donttest{
data('Hepta')
Cls=kmeansClustering(Hepta$Data,7,Type = "Hartigan",nstart=1)
table(Cls$Cls,Hepta$Cls)
ClusterAccuracy(Hepta$Cls,Cls$Cls)
}

\donttest{
data('Hepta')
Cls=kmeansClustering(Hepta$Data,7,Type = "Hartigan",nstart=100)
table(Cls$Cls,Hepta$Cls)
ClusterAccuracy(Hepta$Cls,Cls$Cls)
}
}

\keyword{Accuracy}
\keyword{Clustering}
