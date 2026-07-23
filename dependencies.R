Suggested=c("kernlab", "cclust", "dbscan", "kohonen",
            "MCL", "ADPclust", "cluster", "DatabionicSwarm",
            "orclus", "subspace", "flexclust", "ABCanalysis",
            "apcluster", "pracma", "EMCluster", "pdfCluster", "parallelDist",
            "plotly", "ProjectionBasedClustering", "GeneralizedUmatrix",
            "mstknnclust", "densityClust", "parallel", "energy", "R.utils",
            "tclust", "Spectrum", "genie", "protoclust", "fastcluster", 
            "clusterability", "signal", "reshape2", "PPCI", "clustrd", "smacof",
            "rgl", "prclust","pfclust", "CEC", "dendextend",
            "moments", "prabclus", "VarSelLCM", "sparcl", "mixtools",
            "HDclassif", "clustvarsel", "knitr", "rmarkdown", "igraph",
            "leiden", "clusterSim", "NetworkToolbox", "randomForest", 
            "ConsensusClusterPlus", "RWeka","mlpack","clustMixType","aricode"
)


for(i in 1:length(Suggested)) {
  if (!requireNamespace(Suggested[i], quietly = TRUE)) {
    message(paste("Installing the package", Suggested[i]))
    install.packages(Suggested[i], dependencies = T)
  }
}