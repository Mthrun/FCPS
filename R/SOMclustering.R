SOMclustering=function(Data,LC=c(1,2),ClusterNo=NULL,Mode="online",PlotIt=FALSE,rlen=100,alpha = c(0.05, 0.01),...){
  # INPUT
  # Data[1:n,1:d]     Data set with n observations and d features
  #
  # OPTIONAL
  # LC                Lines and Columns of a very small SOM, usually every unit is a cluster,
  #                   will be ignored if ClusterNo is not NULL.
  # ClusterNo         Number of clusters to search for
  # Mode              Either "batch" or "online"
  # PlotIt            Boolean. Decision to plot or not
  # rlen              Please see kohonen::supersom
  # alpha             Please see kohonen::supersom
  # 
  # OUTPUT
  # Cls[1:n]          Clustering of data
  # Object            Object of kohonen::supersom
  #
  # Author: MT, 04/2018
  if (!requireNamespace('kohonen')) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      list(
        Cls = rep(1, nrow(Data)),
        Object = "Subordinate clustering package is missing.
                Please install the package which is defined in 'Suggests'."
      )
    )
  }
  
  if(missing(LC)){
    if(is.null(ClusterNo)){stop('Either LinesColumns (LC) has to be set or the ClusterNo.')}
  }
	
  if(is.null(ClusterNo)){
    koh=kohonen::supersom(Data,grid = kohonen::somgrid(LC[1],LC[2],...),keep.data=TRUE,mode=Mode,rlen=rlen,alpha=alpha)
    Cls=koh$unit.classif
  }else{
      if(ClusterNo==2){
        LC=c(1,2)
      }
      if(ClusterNo>2&ClusterNo<5){
        LC=c(2,2)
      }
      if(ClusterNo>4&ClusterNo<10){
        LC=c(3,3)
      }
      if(ClusterNo>9&ClusterNo<17){
        LC=c(4,4)
      }
      if(ClusterNo>16&ClusterNo<26){
        LC=c(5,5)
      }
      if(ClusterNo>26&ClusterNo<36){
        LC=c(6,6)
      }
      if(ClusterNo>36){
        LC=c(10,10)
      }
  }
  
  koh=kohonen::supersom(Data,grid = kohonen::somgrid(LC[1],LC[2],...),keep.data=TRUE,mode=Mode,rlen=rlen,alpha=alpha)
  Cls=koh$unit.classif
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=koh))
}