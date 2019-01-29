MoGclustering <-function(Data,ClusterNo=2,PlotIt=FALSE,...){
# Cls <- MoGclustering(Data,ClusterNo);
# call R's Model based clustering or MixtureOfGaussians (MoG) clustering
# INPUT
# Data[1:n]               der Datensatz in Zeilenvektoren 
# ClusterNo    in soviele Cluster werden die daten eingeteilt
#
# OUTPUT List V with
# Cls[1:n]                k-means Clusterung der Daten
# MT 2017
# Übersicht/Kurz-Zfssg in  [Thrun, 2017, p. 23]
#
#  [Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2017.
# Algorithmus aus  
#  [Fraley/Raftery, 2002]  Fraley, C., & Raftery, A. E.: Model-based clustering, discriminant analysis, and density estimation, Journal of the American Statistical Association, Vol. 97(458), pp. 611-631. 2002.
  
#  [Fraley/Raftery, 2006]  Fraley, C., & Raftery, A. E.MCLUST version 3: an R package for normal mixture modeling and model-based clustering,DTIC Document, 2006.
  
  if (ClusterNo<2){
    warning("ClusterNo should to be an integer > 2. Now, all of your data is in one cluster.")
    if(is.null(nrow(Data))){# dann haben wir einen Vektor
      return(cls <- rep(1,length(Data)))
    }else{ # Matrix
      return(cls <- rep(1, nrow(Data)))
    }
  }
  
  requireNamespace('mclust')

res=mclust::Mclust(Data,G=ClusterNo,modelNames=mclust::mclust.options("emModelNames"),...)
if(PlotIt){
  requireNamespace('DataVisualizations')
  DataVisualizations::plot3D(Data,res$classification)
}
return(list(Cls=res$classification,MClustObject=res))
}