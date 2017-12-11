\name{clusternumbers}
\alias{clusternumbers}
\title{Berechnung von Kennzahlen und die darauf basierenden empfehlungen fuer die Anzahl von Klassen in Datensaetzen
}
\description{
Zu einem gegebenen Datensatz und Clusterungen fuer diesen Datensatz werden in Details genannten Kennzahlen berechnet und darauf basierend fuer jede Kennzahl eine Empfehlung bezueglich der Klassenanzahl gegeben.
}
\usage{
clusternumbers(data, cls, max.nc)
clusternumbers(data, max.nc, method)
clusternumbers(data, cls = NULL, max.nc, method = NULL, index = "all", min.nc = 2, print.status = FALSE)
}
\arguments{
  \item{data}{
    die Daten als Matrix
  }
  \item{cls}{
    Clusterungen der zu ueberpruefenden Klassenanzahlen als Matrix mit einer Clusterung pro Spalte (siehe auch Details           (2) und (3)), muss angegeben werden, wenn method = NULL
  }
  \item{max.nc}{
    hoechste Klassenanzahl, die ueberprueft werden soll
  }
  \item{method}{
    Clusterverfahren, mit dem die Clusterungen erstellt werden (siehe Details (4) fuer moegliche Methoden), muss angegeben werden, wenn cls = NULL
  }
  Optional:
  \item{index}{
    String oder Vektor von Strings mit den Kennzahlen die berechnet werden sollen (siehe Details (1)), Standard = "all"
  }
  \item{min.nc}{
    niedrigste Klassenanzahl, die ueberprueft werden soll, Standard = 2
  }
  \item{print.status}{
    wenn TRUE werden Statusmeldungen ausgeben, Standard = FALSE
  }
}
\details{
(1)

Es koennen folgende 26 Kennzahlen berechnet werden:
"calinski", "cindex", "db", "hartigan",
"ratkowsky", "scott", "marriot", "ball", "trcovw", "tracew",
"friedman", "rubin", "ssi", "xuindex", "kl", "ccc", "silhouette",
"duda", "pseudot2", "beale", "ptbiserial", "frey",
"mcclain", "dunn", "sdindex", "sdbw"

Diese koennen ueber den Parameter index einzeln oder als Vektor angegeben werden.
Bei Eingabe 'all' werden alle Kennzahlen berechnet.

(2)

Die Kennzahlen kl, duda, pseudot2, beale, frey und mcclain benoetigen eine Clusterung fuer max.nc+1 Klassen. Sollen diese Kennzahlen berechnet werden, muss diese Clusterung in cls mitangegeben werden.

(3)

Die Kennzahl kl benoetigt eine Clusterung fuer min.nc-1 Klassen. Soll diese Kennzahl berechnet werden, muss dieser Clusterung in cls mitangegeben werden. Fuer den Fall min.nc = 2 muss keine Clusterung fuer 1 angegeben werden.

(4)

Zur Erstellung der Clusterungen koennen folgende Methoden verwendet werden:

"ward.D", "single", "complete", "average", "mcquitty", 
"median", "centroid", "ward.D2", "kmeans", "DBSclustering"

(5)

Die Kennzahlen duda, pseudot2, beale und frey sind nur fuer die Anwendung bei hierarchischen Clusterverfahren gedacht.
}
\value{
\item{Kennzahlen}{
  eine Tabelle der berechneten Kennzahlen
}
\item{Klassenanzahl}{
  fuer jede berechnete Kennzahl die empfohlene Klassenanzahl
}
\item{Clusterungen}{
  Ausgabe der fuer die Berechnung verwendeten Clusterungen
}
\item{criticalValues}{
  die kritischen Werte fuer die Kennzahlen Duda, Pseudot2 und Beale, falls sie berechnet wurden
}
}
\references{
Charrad, Malika, et al. "Package 'NbClust'." J. Stat. Soft 61 (2014): 1-36.

Dimtriadou, E. "cclust: Convex Clustering Methods and Clustering Indexes." R package version 0.6-16, URL \url{http://CRAN.R-project.org/package=cclust} (2009).
}
\author{
Peter Nahrgang
}

\examples{
# Einlesen des Iris-Datensatzes aus dem Standard R-Package datasets
data <- as.matrix(iris[,1:4])

# Erstellen der Clusterungen fuer den Datensatz
#(hier mit Methode complete) fuer die Klassenanzahlen 2 bis 8
hc <- hclust(dist(data), method = "complete")
cls <- matrix(data = 0, nrow = dim(data)[1],  ncol = 7)
for (i in 2:8) {
  cls[,i-1] <- cutree(hc,i)
}

# Berechnung aller Kennzalen und Empfehlungen fuer die Klassenanzahl
clusternumbers(data = data, cls = cls, max.nc = 7)

# Alternativ kann die gleiche Berechnung wie oben mit folgenden Aufruf ausgefzehrt werden:
clusternumbers(data = data, max.nc = 7, method = "complete")
# Bei dieser Variante uebernimmt die Funktion clusternumbers auch die Erstellung der Clusterungen
}
