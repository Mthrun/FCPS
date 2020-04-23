ClusterPlotMDS=function(DataOrDists,Cls,main='Clustering',method = "euclidean",OutputDimension = 3,PointSize=1,Plotter3D="rgl",...){
  
  if(missing(DataOrDists)) stop('DataOrDists is missing.')

  if(is.null(DataOrDists)) stop('DataOrDists is missing.')
  
  if(!is.matrix(DataOrDists)){
    warning('DataOrDists iis not a matrix. Calling as.matrix')
    DataOrDists=as.matrix(DataOrDists)
  }
  
  if(missing(Cls)){
    message('Cls is missing, using default Cls with one cluster.')
    Cls=rep(1,length(DataOrDists))
  }
  if(!is.vector(Cls)){
    warning('Cls is not a vector. Calling as.numeric(as.character(Cls))')
    Cls=as.numeric(as.character(Cls))
  }
  if(OutputDimension>3){
    warning('OutputDimension can be only 2 or 3')
    OutputDimension=3
  } 
  if(OutputDimension<2){
    warning('OutputDimension can be only 2 or 3')
    OutputDimension=3
  } 
  if(nrow(DataOrDists)!=length(Cls)){
    warning('Cls has not the length or DataOrDists, using default Cls with one cluster.')
    Cls=rep(1,length(DataOrDists))
  }
  
  prepareData=function(DataDists,Cls){
    if(requireNamespace('MASS')){
      x=DataDists
      x[upper.tri(x,diag = T)]=NaN
      ind=which(x==0,arr.ind = T)
      if(length(ind)>0){
        DataDists=DataDists[-ind[,1],-ind[,2]]
        Cls=Cls[-ind[,1]]
      }
      DataMDS = MASS::sammon(d = DataDists, y = cmdscale(d = DataDists, 
                                                         k = OutputDimension), k = OutputDimension)$points
    }else{
      DataMDS = cmdscale(d = DataDists, k = OutputDimension, eig = TRUE, 
                         add = FALSE, x.ret = FALSE)$points
    }
    return(list(DataMDS=DataMDS,Cls=Cls))
  }#end prepareData
  
  if (isSymmetric(DataOrDists)) {
    DataDists = DataOrDists
    AnzVar = ncol(DataOrDists)
    AnzData = nrow(DataOrDists)
    
    V=prepareData(DataOrDists,Cls)
    Data=V$DataMDS
    Cls=V$Cls
  }else {
    AnzVar = ncol(DataOrDists)
    AnzData = nrow(DataOrDists)
    if(AnzVar>3){
      DataDists = as.matrix(dist(x = DataOrDists, method = method))
      V=prepareData(DataDists,Cls)
      Data=V$DataMDS
      Cls=V$Cls
    }else{
      Data=DataOrDists
    }#if anzVar>3
  }#if symmetric

  if(requireNamespace('DataVisualizations')){
    Cls[!is.finite(Cls)]=999
    Colors=DataVisualizations::DefaultColorSequence[-2]#no yellow
    Colors=Colors[1:length(unique(Cls))]
    if(Plotter3D=="rgl")
      DataVisualizations::Plot3D(Data = Data,Cls = Cls,UniqueColors = Colors,type="s",size=PointSize,box=F,aspect=T,top=T,main=main,Plotter3D=Plotter3D,...)
    else
      DataVisualizations::Plot3D(Data = Data,Cls = Cls,UniqueColors = Colors,size=PointSize,main=main,Plotter3D=Plotter3D,...)

  }else{
    plot(Data[,1],Data[,2],cols=Cls,main = main,...)
  }

}