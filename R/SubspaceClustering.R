SubspaceClustering <-function(Data,ClusterNo,DimSubspace,method='orclus',PlotIt=FALSE,OrclusInitialClustersNo=ClusterNo+2,...){
# Cls=SubspaceClustering(Data,ClusterNo=2)
#  
# liefert eine Klassenzuweisung
# INPUT
# Data[1:n,1:d]             Der Datensatz

# ClusterNo  in soviele Cluster werden die daten eingeteilt
# PlotIt
# method     'orclus', Subspace Clustering Based on Arbitrarily Oriented Projected Cluster Generation
#               'ProClus'
#               'SubClu'
#               'Clique'
# OrclusInitialClustersNo
# note: JAVA_HOME has to be set for rJava to use this method
# OUTPUT
# Cls[1:n]                Clusterung der Daten
#
# Author: MT 04/2018
  
  #orclus
  d=dim(Data)[2]
  n=d=dim(Data)[1]
  
  switch(method,
         orclus={
           
           if(missing(DimSubspace)){
             if(d>3){
               DimSubspace=dim(Data)[2]-1 
               if(DimSubspace>n/ClusterNo){
                 DimSubspace=n/ClusterNo-1
               }
               DimSubspace=min(c(DimSubspace,20)) #higher subsopace is not computable
             }else{
               DimSubspace=dim(Data)[2]*0.99
             }
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
         SubClu={
           requireNamespace('subspace')
          obj=subspace::SubClu(data=Data, ...)
           
           Cls=rep(NaN,nrow(Data))
           for(i in 1:length(obj)){
             Cls[obj[[i]]$objects]=i
           }
           Cls[!is.finite(Cls)]=9999
         },
         Clique={
           requireNamespace('subspace')
           obj=subspace::CLIQUE(data=Data, ...)
           
           Cls=rep(NaN,nrow(Data))
           for(i in 1:length(obj)){
             Cls[obj[[i]]$objects]=i
           }
           Cls[!is.finite(Cls)]=9999
         },
         stop("Wrong method string entered")
         
  )

if(PlotIt){
  requireNamespace('DataVisualizations')

  DataVisualizations::Plot3D(Data,Cls)
}
return(list(Cls=Cls,SubspaceObject=obj))
}