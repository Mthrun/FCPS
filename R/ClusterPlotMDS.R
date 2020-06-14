ClusterPlotMDS=function(DataOrDists,Cls,main='Clustering',method = "euclidean",OutputDimension = 3,PointSize=1,Plotter3D="rgl",Colorsequence,...){
  #
  # INPUT
  # DataOrDists        Either nonsymmetric [1:n,1:d] datamatrix of n cases and d features or
  #                    symmetric [1:n,1:n] distance matrix
  # Cls                1:n numerical vector of numbers defining the classification as the main
  #                    output of the clustering algorithm for the n cases of data. It has k unique
  #                    numbers representing the arbitrary labels of the clustering.
  # main               String. Title of plot
  # method             Method to compute distances, default "euclidean"
  # OutputDimension    Either two or three depending on user choice
  # PointSize          Size of points l
  # Plotter3D          In case of 3 dimensions, choose either "plotly" or "rgl"
  # 
  # OUTPUT
  # The rgl plot
  # 
  # 
  if(missing(DataOrDists)) stop('DataOrDists is missing.')

  if(is.null(DataOrDists)) stop('DataOrDists is missing.')
  
  if(!is.matrix(DataOrDists)){
    warning('DataOrDists is not a matrix. Calling as.matrix')
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
    
    Cls[!is.finite(Cls)]=999

    if(requireNamespace('smacof')){
      DataMDS =smacof::mds(DataDists,ndim = 3)$conf
      # DataMDS = MASS::sammon(d = DataDists, y = cmdscale(d = DataDists, 
      #                                                    k = OutputDimension), k = OutputDimension)$points
    }else{
      DataMDS = cmdscale(d = DataDists, k = OutputDimension, eig = TRUE, 
                         add = FALSE, x.ret = FALSE)$points
    }
    return(list(DataMDS=DataMDS,Cls=Cls))
  }# End prepareData
  
  if (isSymmetric(unname(DataOrDists))) {
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
      if(requireNamespace('parallelDist')){
        DataDists = as.matrix(parallelDist::parallelDist(x = DataOrDists, method = method))
      }else{
        warning('ClusterPlotMDS: parallelDist package is missing. Using dist()')
        DataDists = as.matrix(dist(x = DataOrDists, method = method))
      }
      V=prepareData(DataDists,Cls)
      Data=V$DataMDS
      Cls=V$Cls
    }else{
      Data=DataOrDists
    }#if anzVar>3
  }#if symmetric

  numberOfClasses=length(unique(Cls))
  
  if(missing(Colorsequence)){
    if(requireNamespace("DataVisualizations")){
      Colors= DataVisualizations::DefaultColorSequence
      Colors=Colors[1:numberOfClasses]
    }
    else{
      stop('DataVisualizations package not loaded or installed. Please provide Colorsequence manually.')
    }
  }else{
    Colors=Colorsequence
    if(length(Colors)!=numberOfClasses){
      warning('Default color sequence is used, because the number of colors does not equal the number of clusters.')
      if(requireNamespace("DataVisualizations")){
        Colors= DataVisualizations::DefaultColorSequence[1:numberOfClasses]
      }
      else{
        stop('DataVisualizations package not loaded or installed. Please provide Colorsequence manually.')
      }
    }
  }
  
  if(requireNamespace('DataVisualizations')){
    if(Plotter3D=="rgl"){
      if(dim(Data)[2]!=2){
        return(DataVisualizations::Plot3D(Data = Data,
                                          Cls = Cls,
                                          UniqueColors = Colors,
                                          type="s",
                                          size=PointSize,
                                          box=F,
                                          aspect=T,
                                          top=T,
                                          main=main,
                                          Plotter3D=Plotter3D,...))
      }
      else{
        p=DataVisualizations::Plot3D(Data = Data,
                                     Cls = Cls,
                                     UniqueColors = Colors,
                                     size=PointSize,
                                     Plotter3D=Plotter3D,...)+ggplot2::ggtitle(main)
        print(p)
        return(p)
      }
  
    }else{
      if(dim(Data)[2]!=2){
        p=DataVisualizations::Plot3D(Data = Data,Cls = Cls,UniqueColors = Colors,size=PointSize,Plotter3D=Plotter3D,...)
        p=plotly::layout(p,title=main)
        p
        return(p)
      }else{
        return(DataVisualizations::Plot3D(Data = Data,
                                          Cls = Cls,
                                          UniqueColors = Colors,
                                          size=PointSize,
                                          Plotter3D=Plotter3D,...)+ggplot2::ggtitle(main))
      }
    }
  }else{
    plot(Data[,1],Data[,2],cols=Cls,main = main,...)
  }
}