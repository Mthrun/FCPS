AutomaticProjectionBasedClustering=function(DataOrDistances,ClusterNo,Type="NerV",StructureType = TRUE,PlotIt=FALSE,PlotTree=FALSE,PlotMap=FALSE,...){
  #author: MT, 04/2020
  if(!requireNamespace('ProjectionBasedClustering')){
    warning('ProjectionBasedClustering is not installed.')
    return('ProjectionBasedClustering is not installed.')
  }
  
  if(isSymmetric(unname(DataOrDistances))){
    Type %in% c('Sammon','Pswarm','MDS')
    if(!(Type %in% c('Sammon','Pswarm','MDS'))){
      Type='MDS'
      warning('Distances Matrix is given but the Type',Type,'is selected, which does not work with distances. Switching to MDS.')
    }
   
     message('Given a Distance Matrix instead of Data is experimental. MDS transformation is used to generated a Data Matrix.')
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
        warning('Incorrect Type Selected')
        return('Incorrect Type Selected')
      }
    )
  #  out=out

  #Computation of Generalized Umatrix
  if(Type!='Pswarm')
    visualization=GeneralizedUmatrix::GeneralizedUmatrix(Data = Data,out$ProjectedPoints,PlotIt = FALSE)
  else
    visualization=DatabionicSwarm::GeneratePswarmVisualization(Data = Data,out$ProjectedPoints,out$LC,PlotIt = FALSE)
  # Visualizuation of GenerelizedUmatrix

  # Automatic Clustering
  if(Type!='Pswarm'){
    LC=c(visualization$Lines,visualization$Columns)
    # number of Cluster from dendrogram or visualization (PlotIt=T)
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