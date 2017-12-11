\name{kmeansdist}
\alias{kmeansdist}
\title{
K-Means Clustering over a distance matrix
}
\description{
Perform k-means clustering on a distance matrix
}
\usage{
kmeansdist(centers, distances, maxIt = 2000, verbose = F)
}
\arguments{
  \item{distances}{ Distanzmatrix. Bei n Datenpunkten der Dimension n x n  }
  \item{centers}{Entweder: Vektor aus Indizes <= n die initiale Zentroide fuer Kmeans sind. Oder: Integer, gibt an wieviele Zentroide es geben soll. Diese werden dann zuf?llig gezogen.}
  \item{maxIt}{ Optional: Maximale Anzahl an Iterationen bevor der Algorithmus abbricht.}
  \item{Verbose}{Optional: Algorithmus gibt immer aktuelle Iteration aus.}
}

\value{
  \item{centerids[1:k]}{Indizes der Zentroide aus denen die Clusterung Cls erstellt wurde}
  \item{Cls[1:n] }{k-means Clusterung der Daten}

}
\author{
Felix Pape
}
