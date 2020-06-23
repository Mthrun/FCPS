[![Travis-CI Build Status](https://travis-ci.org/privefl/bigstatsr.svg?branch=master)](https://travis-ci.org/privefl/bigstatsr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/privefl/bigstatsr?branch=master&svg=true)](https://ci.appveyor.com/project/privefl/bigstatsr)
[![Coverage Status](https://img.shields.io/codecov/c/github/privefl/bigstatsr/master.svg)](https://codecov.io/github/privefl/bigstatsr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/FCPS)](https://cran.r-project.org/package=FCPS)
[![DOI](https://zenodo.org/badge/doi/10.1093/bioinformatics/bty185.svg)](https://doi.org/10.1093/bioinformatics/bty185)


# FCPS

*Fundamental Clustering Problems Suite*

A R package for clustering algorithms in machine learning (AI)

## Table of contents



1. [Description](#description)
2. [Use cases](#use-cases)
3. [Installation](#installation)
4. [Manual](#manual)
5. [References](#references)
6. [Additional information](#additional-information)


## Description

The Fundamental Clustering Problems Suite (short FCPS) is a project of the faculty of Data Bionics at the university of Marburg.
The Fundamental Clustering Problems Suite (FCPS) summaries 54 state-of-the-art clustering algorithms available in R language. An important advantage is that the input and output of clustering algorithms is simplified and consistent in order to enable users a swift execution of cluster analysis. By combining mirrored-density plots (MD plots) with statistical testing FCPS provides a tool to investigate the cluster tendency quickly prior to the cluster analysis itself
[![[Thrun 2020]](https://doi.org/10.2312/mlvis.20201102)](https://doi.org/10.2312/mlvis.20201102).
Common clustering challenges can be generated with arbitrary sample size
[![[Thrun and Ultsch 2020]](https://doi.org/10.1016/j.dib.2020.105501)](https://doi.org/10.1016/j.dib.2020.105501).
Additionally, FCPS sums 26 indicators with the goal to estimate the number of clusters up and provides an appropriate implementation of the clustering accuracy for more than two clusters
[![[Thrun and Ultsch 2020]](https://doi.org/10.1016/j.artint.2020.103237)](https://doi.org/10.1016/j.artint.2020.103237).
A subset of methods was used in a benchmarking of algorithms published in
[![[Thrun and Ultsch 2020]](missingdoi)](missingdoi).


## Use cases

### Visualization of high dimensional data

```R
library(FCPS)
data("Leukemia")
Data=Leukemia$Distance
Cls=Leukemia$Cls
ClusterPlotMDS(Data,Cls,main = ’Leukemia’,Plotter3D = ’plotly’)
```


<img src="/img/Fig0.png" width="400" height="400">

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
![Visualization](https://user-images.githubusercontent.com/31764814/85043227-68cca000-b18c-11ea-822a-528f55227025.png){:height="200px" width="100px"}

<img src="/img/Fig1.png" width="400" height="400">


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

![Visualization](https://user-images.githubusercontent.com/31764814/85043244-6f5b1780-b18c-11ea-96d2-3f0d8ccc400d.png| width=100)

<img src="/img/Fig2.png" width="400" height="400">

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

![Visualization](https://user-images.githubusercontent.com/31764814/85057641-5446d280-b1a1-11ea-8f50-4b9760d4eb5f.png| width=100)

<img src="/img/Fig3.png" width="400" height="400">


## Installation

#### Installation using Github


```R
remotes::install_github("Mthrun/FCPS")
```

#### Installation using R Studio

*Tools -> Install Packages -> Repository (CRAN) -> FCPS*


## References

1. [Thrun, 2020] Thrun, M. C.: Improving the Sensitivity of Statistical Testing for Clusterability with Mirrored-Density Plots, Eurographics, 2020.
2. [Thrun/Ultsch, 2020] Thrun, M. C., & Ultsch, A.: Clustering benchmark datasets exploiting the fundamental clustering problems, Data in Brief, 2020.
3. [Thrun/Ultsch, 2020] Thrun, M. C., & Ultsch, A.: Swarm intelligence for self-organized clustering, Artificial Intelligence, 2020.
4. [Thrun/Ultsch, 2020]  Thrun, M. C., & Ultsch, A.: Using Projection based Clustering to Find Distance and Density based Clusters in High-Dimensional Data, Journal of Classification, accepted, 2020.


## Manual

An elaborating manual can be found on CRAN, where the package is published:

https://cran.r-project.org/web/packages/FCPS/FCPS.pdf

## Additional information

| Authors website  | http://www.deepbionics.org/           |
| ---------------- |--------------------------------------:|
| License          | GPL-3                                 |
| Dependencies     | R (>= 3.5.0)                          |
| Bug reports      | https://github.com/Mthrun/FCPS/issues |
