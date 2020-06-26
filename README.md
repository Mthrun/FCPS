[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FCPS)](https://cran.r-project.org/package=FCPS)
[![DOI](https://zenodo.org/badge/113846715.svg)](https://zenodo.org/badge/latestdoi/113846715)

# FCPS

*Fundamental Clustering Problems Suite*

The package provides many clustering algorithms for unsupervised machine learning.

## Table of contents

1. [Description](#description)
2. [Use cases](#use-cases)
3. [Installation](#installation)
4. [Manual](#manual)
5. [Documentation](#documentation)
6. [Additional information](#additional-information)
7. [References](#references)

## Description

The Fundamental Clustering Problems Suite (FCPS) summaries 54 state-of-the-art clustering algorithms available in R language. An important advantage is that the input and output of clustering algorithms is simplified and consistent in order to enable users a swift execution of cluster analysis. By combining mirrored-density plots (MD plots) with statistical testing FCPS provides a tool to investigate the cluster tendency quickly prior to the cluster analysis itself
[![[Thrun 2020]](https://doi.org/10.2312/mlvis.20201102)](https://doi.org/10.2312/mlvis.20201102).
Common clustering challenges can be generated with arbitrary sample size
[![[Thrun and Ultsch 2020a]](https://doi.org/10.1016/j.dib.2020.105501)](https://doi.org/10.1016/j.dib.2020.105501).
Additionally, FCPS sums 26 indicators with the goal to estimate the number of clusters up and provides an appropriate implementation of the clustering accuracy for more than two clusters
[![[Thrun and Ultsch 2020b]](https://doi.org/10.1016/j.artint.2020.103237)](https://doi.org/10.1016/j.artint.2020.103237).
A subset of methods was used in a benchmarking of algorithms published in
[![[Thrun and Ultsch 2020c]](missingdoi)](missingdoi).


## Use cases

### Visualization of high dimensional data

```R
library(FCPS)
data("Leukemia")
Data=Leukemia$Distance
Cls=Leukemia$Cls
ClusterPlotMDS(Data,Cls,main = ’Leukemia’,Plotter3D = ’plotly’)
```

<p align="center">
  <img src="/img/Fig0.png" width="400" height="400">
</p>


### Cluster challenge - Testing clustering algorithms on artificial data sets

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

<p align="center">
  <img src="/img/Fig1.png" width="400" height="400">
</p>

### Clusterability / Cluster tendency - Estimating the potentiality of data sets to be clustered

```R
library(FCPS)
set.seed(600)
DataList=ClusterChallenge("Chainlink",SampleSize = 750)
Data=DataList$Chainlink
Cls=DataList$Cls
library(ggplot2)
ClusterabilityMDplot(Data)+theme_bw()
```

<p align="center">
  <img src="/img/Fig2.png" width="400" height="400">
</p>


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


<p align="center">
  <img src="/img/Fig4.png" width="400" height="200">
</p>

## Installation

#### Installation using Github


```R
remotes::install_github("Mthrun/FCPS")
```

#### Installation using R Studio

*Tools -> Install Packages -> Repository (CRAN) -> FCPS*

## Tutorial Examples

The tutorial with several examples can be found on in the vignette on CRAN:

https://cran.r-project.org/web/packages/FCPS/vignettes/FCPS.html

## Manual

The full manual for users or developers is available here:
https://cran.r-project.org/web/packages/FCPS/FCPS.pdf

## Additional information

| Authors website  | http://www.deepbionics.org/           |
| ---------------- |--------------------------------------:|
| License          | GPL-3                                 |
| Dependencies     | R (>= 3.5.0)                          |
| Bug reports      | https://github.com/Mthrun/FCPS/issues |


## References

1. [Thrun, 2020] Thrun, M. C.: Improving the Sensitivity of Statistical Testing for Clusterability with Mirrored-Density Plot, in Archambault, D., Nabney, I. & Peltonen, J. (eds.), Machine Learning Methods in Visualisation for Big Data, DOI 10.2312/mlvis.20201102, The Eurographics Association, Norrköping , Sweden, May, 2020. 

2. [Thrun/Ultsch, 2020a] Thrun, M. C., & Ultsch, A.: Clustering Benchmark Datasets Exploiting the Fundamental Clustering Problems, Data in Brief,Vol. 30(C), pp. 105501, DOI 10.1016/j.dib.2020.105501 , 2020.
3. [Thrun/Ultsch, 2020b]  Thrun, M. C., & Ultsch, A.: Swarm Intelligence for Self-Organized Clustering, Journal of Artificial Intelligence, in press, DOI: 10.1016/j.artint.2020.103237, 2020.
4. [Thrun/Ultsch, 2020c]  Thrun, M. C., & Ultsch, A. : Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, accepted, Springer, 2020.

