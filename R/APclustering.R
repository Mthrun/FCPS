APclustering=function(DataOrDistances,InputPreference=NA,ExemplarPreferences=NA,DistanceMethod="euclidean",Seed=7568,PlotIt=FALSE,Data,...){
#Cls=APcluster(Data,Seed=7568)$Cls
#Affinity Propagation clustering introduced by Frey and Dueck (2007) <doi:10.1126/science.1136800>.
#INPUT
#DataOrDistances[1:n,1:l], if l=n and symmetric then distance matrix ausumed, otherwise l=k with k variables
#Optional
# InputPreference   see \code{apcluster}
# ExemplarPreferencessee \code{apcluster}

# Seed   see \code{apcluster}
#OUTPUT
# Cls[1:n]
# APresults class of apcluster
#
# author MT: 04/2018

#note: # NoNoise   see \code{apcluster}
  if (!requireNamespace('apcluster')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(DataOrDistances)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }

  if(missing(DataOrDistances)){
    DataOrDistances=Data
  }
  
  if(!is.matrix(DataOrDistances)){
    warning('DataOrDistances is not a matrix. Calling as.matrix()')
    DataOrDistances=as.matrix(DataOrDistances)
  }

  if(!mode(DataOrDistances)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
    mode(DataOrDistances)='numeric'
  }
  AnzData = nrow(DataOrDistances)

  if (isSymmetric(unname(DataOrDistances))) {
    s=-(DataOrDistances)^2
    apres <- apcluster::apcluster(s=s,p=InputPreference, details=TRUE,q=ExemplarPreferences,seed=Seed,...)

  }
  else{
    s=DataOrDistances
    apres <- apcluster::apcluster(apcluster::negDistMat(method = DistanceMethod,r=2), x=DataOrDistances,p=InputPreference,q=ExemplarPreferences, details=TRUE,seed=Seed,...)
  }
  ClsIndList=apres@clusters
  Cls=rep(NaN,AnzData)
  for(i in 1:length(ClsIndList)){
    Cls[ClsIndList[[i]]]=i  
  }
  if(PlotIt){
	ClusterPlotMDS(DataOrDistances,Cls)
  }
  Cls=ClusterRename(Cls,DataOrDistances)
  return(list(Cls=Cls,Object=apres))
}