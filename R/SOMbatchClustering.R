SOMbatchClustering=function(Data,LC=c(3,3),PlotIt=FALSE,...){
  
  #author: MT, 04/2018
  requireRpackage('kohonen')
  koh=kohonen::supersom(Data,grid = kohonen::somgrid(LC[1],LC[2]),keep.data=TRUE,...)
  Cls=koh$unit.classif
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(PlotIt){
    requireNamespace('DataVisualizations')
    DataVisualizations::plot3D(Data,Cls)
  }
  return(list(Cls=Cls,ObjectKohonen=koh))
}