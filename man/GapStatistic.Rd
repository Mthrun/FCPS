\name{GapStatistic}
\alias{GapStatistic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gap Statistic
}
\description{
Gap Statistic
}
\usage{
GapStatistic(Data, ClusterNoMax, ClusterFun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
%%     ~~Describe \code{Data} here~~
}
  \item{ClusterNoMax}{
%%     ~~Describe \code{ClusterNoMax} here~~
}
  \item{ClusterFun}{
%%     ~~Describe \code{ClusterFun} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Tibshirani, R., Walther, G. and Hastie, T: Estimating the number of data clusters via the Gap statistic, Journal of the Royal Statistical Society B,63, 411–423, 2003.
}
\author{
Michael Thrun
}
\note{
Wrapper only
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Hepta)
GapStatistic(Hepta$Data,10,ClusterFun = kmeans)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
