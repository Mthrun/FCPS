# FCPS
Fundamental Clustering Problems Suite

## Description

The Fundamental Clustering Problems Suite (short FCPS) is a project of the faculty of Data Bionics at the university of Marburg.
FCPS summarizes 54 state-of-the-art clustering algorithms available in R language. An important advantage is that the input
and output of clustering algorithms is standardized to enable users a swift execution of cluster analysis. By combining
mirrored-density plots (MD plots) with statistical testing FCPS provides a tool to investigate the cluster tendency quickly
prior to the cluster analysis itself. Common clustering challenges can be generated with arbitrary sample size. Additionally,
FCPS sums 26 indicators with the goal to estimate the number of clusters up and provides an appropriate implementation of the 
clustering accuracy for more than two clusters.


## Use cases

### Visualization

```R
library(FCPS)
data("Leukemia")
Data=Leukemia$Distance
Cls=Leukemia$Cls
ClusterPlotMDS(Data,Cls,main = ’Leukemia’,Plotter3D = ’plotly’)
```

### Cluster challenge

```R
set.seed(600)
library(FCPS)
DataList=ClusterChallenge("Chainlink", SampleSize = 750,
PlotIt=TRUE)
Data=DataList$Chainlink
Cls=DataList$Cls
> ClusterCount(Cls)
$CountPerCluster
$NumberOfClusters
$ClusterPercentages
[1] 377 373
[1] 2
[1] 50.26667 49.73333
```

### Clusterability / Cluster tendency

```R
library(FCPS)
set.seed(600)
DataList=ClusterChallenge("Chainlink",SampleSize = 750)
Data=DataList$Chainlink
Cls=DataList$Cls
library(ggplot2)
ClusterabilityMDplot(Data)+theme_bw()
```


### Estimation of number of clusters

```R
library(FCPS)
set.seed(135)
DataList=ClusterChallenge("Chainlink",SampleSize = 900)
Data=DataList$Chainlink
Cls=DataList$Cls
Tree=HierarchicalClustering(Data,0,"SingleL")[[3]]
ClusterDendrogram(Tree,4,main=’Single Linkage’)
MaximumNumber=7
clsm <- matrix(data = 0, nrow = dim(Data)[1], ncol = MaximumNumber)
for (i in 2:(MaximumNumber+1)) {
clsm[,i-1] <- cutree(Tree,i)
}
out=ClusterNoEstimation(Data, ClsMatrix = clsm,
max.nc = MaximumNumber, PlotIt = TRUE)
```










