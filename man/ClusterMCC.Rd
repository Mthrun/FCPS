\name{ClusterMCC}
\alias{ClusterMCC}

\title{
Matthews Correlation Coefficient (MCC)
}
\description{
Matthews correlation coefficient eneralized to the multiclass case (a.k.a.  R_K statistic).
}
\usage{
ClusterMCC(PriorCls, CurrentCls)
}

\arguments{
\item{PriorCls}{
Ground truth,[1:n] numerical vector with n numbers defining the classification. It has k unique numbers representing the labels of the clustering.
}
\item{CurrentCls}{
Main output of the clustering, [1:n]  numerical vector with n numbers defining the classification. It has k unique numbers representing the labels of the clustering.
}
}
\details{
Contrary to accuracy, the MCC is balanced measure which can be used even if the classes are of very different sizes. When there are more than two labels the MCC will no longer range between -1 and +1. Instead the minimum value will be between -1 and 0 depending on the true distribution. The maximum value is always +1.
Beware that in contrast to \code{\link{ClusterAccuracy}}, the labels cannot be arbitrary. Instead each label of \code{PriorCls} and \code{CurrentCls} has to be mapped to the same cluster of data points. Typically this has to be ensured manually.
}
\value{
Single scalar of MCC in a range described in details.
}
\references{
Matthews, B. W.: Comparison of the predicted and observed secondary structure of T4 phage lysozyme, Biochimica et Biophysica Acta (BBA), Protein Structure, Vol. 405(2), pp. 442-451, 1975.

Boughorbel, S.B: Optimal classifier for imbalanced data using Matthews Correlation Coefficient metric, PLOS ONE, Vol. 12(6), pp. e0177678, 2017.

Chicco, D.; Toetsch, N. and Jurman, G.: The Matthews correlation coefficient (MCC) is more reliable than balanced accuracy, bookmaker informedness, and markedness in two_class confusion matrix evaluation. BioData Mining. Vol. 14., 2021.
}
\author{
Michael Thrun
}
\note{
If No. of Clusters is not equivalent, internally the number is allgined with zero datapoints belonging to the missing clusters.
}

\seealso{
\code{\link{ClusterAccuracy}}
}
\examples{
#Beware that algorithm arbitrary defines the labels
data(Hepta)
V=kmeansClustering(Hepta$Data,Type = "Hartigan",7)
table(V$Cls,Hepta$Cls)
#result is only valid if the above issue is resolved manually
ClusterMCC(Hepta$Cls,V$Cls)
}

\keyword{MCC}
\keyword{Matthews}

\concept{Matthews Correlation Coefficient}
\concept{Matthews Correlation}
\concept{Rk statistic}
