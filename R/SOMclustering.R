SOMclustering=function(Data,LC=c(1,2),ClusterNo=NULL,Mode="online",PlotIt=FALSE,rlen=100,alpha = c(0.05, 0.01),...){
  
  #author: MT, 04/2018
  requireNamespace('kohonen')
  
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
    requireNamespace('DataVisualizations')
    DataVisualizations::Plot3D(Data,Cls)
  }
  return(list(Cls=Cls,Object=koh))
}