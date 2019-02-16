MoGclustering <-function(Data,ClusterNo=2,method="EM",PlotIt=FALSE,...){
# Cls <- MoGclustering(Data,ClusterNo);
# MixtureOfGaussians (MoG) clustering using EM
# INPUT
# Data[1:n]               der Datensatz in Zeilenvektoren 
# ClusterNo    in soviele Cluster werden die daten eingeteilt
#
# OUTPUT List V with
# Cls[1:n]                k-means Clusterung der Daten
# MT 2017
# IMPORTANT UPDATE: MoGclustering renamed to ModelBasedClustering MoG Clustering is now defined es Mixture of Gaussians based on EM This is a change contrary to my PhD thesis [Thrun, 2018]! Additionally density based clustering methods added.
#

  if (ClusterNo<2){
    warning("ClusterNo should to be an integer > 2. Now, all of your data is in one cluster.")
    if(is.null(nrow(Data))){# dann haben wir einen Vektor
      return(cls <- rep(1,length(Data)))
    }else{ # Matrix
      return(cls <- rep(1, nrow(Data)))
    }
  }#
  requireNamespace('EMCluster')
  switch(method,
    EM={
      out=EMCluster::starts.via.svd(Data, nclass = ClusterNo, method = c("em"),
                                    EMC = EMCluster::.EMC)
    },
    kmeans={
      out=EMCluster::emgroup(x = Data,nclass = ClusterNo,EMC = EMCluster::.EMC)
    },{
      stop('Please choose either "kmeans" or "EM".')
    }
  )


Cls=as.vector(out$class)

if(!is.null(rownames(Data)))
  names(Cls)=rownames(Data)

if(PlotIt){
  requireNamespace('DataVisualizations')
  DataVisualizations::Plot3D(Data,Cls)
}
return(list(Cls=Cls,MClustObject=out))
}