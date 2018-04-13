APclustering=function(DataOrDistances,InputPreference=NA,ExemplarPreferences=NA,Seed,PlotIt=FALSE,...){
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

  requireNamespace('apcluster')
  if(!is.matrix(DataOrDistances)){
    warning('DataOrDistances is not a matrix. Calling as.matrix()')
    DataOrDistances=as.matrix(DataOrDistances)
  }
  if(!mode(DataOrDistances)=='numeric'){
    warning('Data is not a numeric matrix. Calling mode(DataOrDistances)="numeric"')
    mode(DataOrDistances)='numeric'
  }
  AnzData = nrow(DataOrDistances)
  
  if (isSymmetric(DataOrDistances)) {
    s=(1-DataOrDistances)/max(DataOrDistances)
    apres <- apcluster::apcluster(s=s,p=InputPreference, details=TRUE,q=ExemplarPreferences,seed=Seed,...)
    if(PlotIt){
      PlotIt=FALSE
      warning('Only a Input of a dataset can be visualized.')
    } 
  }
  else{
    s=DataOrDistances
    apres <- apcluster::apcluster(apcluster::negDistMat(r=2), x=DataOrDistances,p=InputPreference,q=ExemplarPreferences, details=TRUE,seed=7568,...)
  }
  ClsIndList=apres@clusters
  Cls=rep(NaN,AnzData)
  for(i in 1:length(ClsIndList)){
    Cls[ClsIndList[[i]]]=i  
  }
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(DataOrDistances,Cls)
  }
  return(list(Cls=Cls,APobject=apres))
}