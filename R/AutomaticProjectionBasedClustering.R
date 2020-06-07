AutomaticProjectionBasedClustering=function(DataOrDistances,ClusterNo,Type="NerV",StructureType = TRUE,PlotIt=FALSE,PlotTree=FALSE,PlotMap=FALSE,...){
  #
  # INPUT
  # DataOrDistances[1:n,1:d]    Dataset with n observations and d features or distance matrix with size n
  # ClusterNo                   Number of clusters to search for
  # Type                        Type of Projection method. Choose: NerV, Pswarm, MDS, ICA, CCA, Sammon
  # StructureType               Boolean. Either compact (TRUE) or connected (FALSE), see discussion in [Thrun, 2018] 
  # PlotIt                      Boolean. Decision to plot or not
  # PlotTree                    Boolean. Plots the dendrogram
  # PlotMap                     Boolean. Plots the topographic map [Thrun et al., 2016].
  # 
  # OUTPUT
  # Cls[1:n]    Clustering of data
  # Object      List of projection and visualization
  #
  # Author: MT, 04/2020
  
  if (!requireNamespace('ProjectionBasedClustering')) {
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
  
  if(isSymmetric(unname(DataOrDistances))){
    Type %in% c('Sammon','Pswarm','MDS')
    if(!(Type %in% c('Sammon','Pswarm','MDS'))){
      Type='MDS'
      warning('Distances matrix is given but the type',Type,'is selected, which does not work with distances. Switching to MDS.')
    }
   
    message('Given a distance matrix instead of data is experimental. MDS transformation is used to generate a data matrix.')
    Data=ProjectionBasedClustering::MDS(DataOrDists = DataOrDistances,OutputDimension = dim(DataOrDistances)[1]-2)$ProjectedPoints
    
  }else{
    Data=DataOrDistances
  }

    switch(Type,
      'NerV'={
        out=list()
        out$ProjectedPoints=ProjectionBasedClustering::NeRV(Data = DataOrDistances,OutputDimension = 2,...)
        },
      'Pswarm'={
        if(requireNamespace('DatabionicSwarm')){
          out=DatabionicSwarm::Pswarm(DataOrDistance = DataOrDistances,...)
        }else{
          warning('DatabionicSwarm is not installed.')
          return('DatabionicSwarm is not installed.')
        }
         
        },
      'MDS'={out=ProjectionBasedClustering::MDS(DataOrDists = DataOrDistances,OutputDimension = 2,...)
      
      },
      'CCA'={
        out=ProjectionBasedClustering::CCA(DataOrDists = DataOrDistances,OutputDimension = 2,...)
      },
      'Sammon'={
        out=ProjectionBasedClustering::SammonsMapping(DataOrDists = DataOrDistances,OutputDimension = 2,...)
      },
      'ICA'={
        out=ProjectionBasedClustering::ICA(Data = DataOrDistances,OutputDimension = 2,...)
      },{
        warning('Incorrect type selected')
        return('Incorrect type selected')
      }
    )
  #  out=out

  # Computation of GeneralizedUmatrix
  if(Type!='Pswarm')
    if(requireNamespace("GeneralizedUmatrix")){
      visualization=GeneralizedUmatrix::GeneralizedUmatrix(Data = Data,out$ProjectedPoints,PlotIt = FALSE)
    }
    else{
      stop('GeneralizedUmatrix package not loaded or installed.')
    }
  else{
    if(requireNamespace("DatabionicSwarm")){
      # Visualization of GenerelizedUmatrix
      visualization=DatabionicSwarm::GeneratePswarmVisualization(Data = Data,out$ProjectedPoints,out$LC,PlotIt = FALSE)
    }
    else{
      stop('DatabionicSwarm package not loaded or installed.')
    }
  }

  # Automatic Clustering
  if(Type!='Pswarm'){
    LC=c(visualization$Lines,visualization$Columns)
    # Number of cluster from dendrogram or visualization (PlotIt=T)
    Cls=ProjectionBasedClustering::ProjectionBasedClustering(k=ClusterNo, Data = Data, BestMatches = visualization$Bestmatches, LC,StructureType = StructureType,PlotIt=PlotTree)
  }else{
    Cls=DatabionicSwarm::DBSclustering(k = ClusterNo,DataOrDistance = DataOrDistances,BestMatches = visualization$Bestmatches,LC = visualization$LC,StructureType = StructureType,PlotIt = PlotTree)
  }
  # Verification
  if(isTRUE(PlotMap)){
	if(requireNamespace('GeneralizedUmatrix'))
		GeneralizedUmatrix::plotTopographicMap(visualization$Umatrix,visualization$Bestmatches,Cls)
  }
  if(isTRUE(PlotIt)){
    ClusterPlotMDS(Data,Cls)
  }
  Cls=ClusterRename(Cls,DataOrDistances)
    return(list(Cls=Cls,Object=list(Projection=out,GeneralizedUmatrix=visualization)))
  }