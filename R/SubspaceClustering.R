SubspaceClustering <-function(Data,ClusterNo,DimSubspace,PlotIt=FALSE,Algorithm='orclus',OrclusInitialClustersNo=2*ClusterNo,...){
# Cls=SubspaceClustering(Data,ClusterNo=2)
#  
# liefert eine Klassenzuweisung
# INPUT
# Data[1:n,1:d]             Der Datensatz

# ClusterNo  in soviele Cluster werden die daten eingeteilt
# PlotIt
# Algorithm     'orclus', Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generation
#               'ProClus' 
# OrclusInitialClustersNo
# note: JAVA_HOME has to be set for rJava to use this algorithm
# OUTPUT
# Cls[1:n]                Clusterung der Daten
#
# Author: MT 04/2018
  
  #orclus

  switch(Algorithm,
         orclus={
           d=dim(Data)[2]
           if(missing(DimSubspace)){
             if(d>3)
               DimSubspace=dim(Data)[2]-1    
             else
               DimSubspace=dim(Data)[2]*0.99
           }
           requireNamespace('orclus')
           obj=orclus::orclus(x=Data, k=ClusterNo,l=DimSubspace,k0=OrclusInitialClustersNo, ...)
           Cls=obj$cluster
         },
         ProClus ={
           requireNamespace('subspace')
           if(!missing(DimSubspace))
            obj=subspace::ProClus(data=Data, k = ClusterNo,d=DimSubspace,...)
           else
             obj=subspace::ProClus(data=Data, k = ClusterNo,...)
           
           Cls=rep(NaN,nrow(Data))
          for(i in 1:length(obj)){
            Cls[obj[[i]]$objects]=i
          }
           Cls[!is.finite(Cls)]=9999
         },
         stop("Wrong Algorithm string entered")
         
  )

if(PlotIt){
  requireNamespace('DataVisualizations')

  DataVisualizations::plot3D(Data,Cls)
}
return(list(Cls=Cls,SubspaceObject=obj))
}