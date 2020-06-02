ClusterChallenge=function(Name,SampleSize,PlotIt=FALSE,PointSize=1,Plotter3D="rgl",...){
  #
  # INPUT
  # Name          String. Choose: 'Atom', 'Chainlink, 'EngyTime', 'GolfBall', 'Hepta',
  #                               'Lsun3D', 'Target', 'Tetra', 'TwoDiamonds', 'WingNut
  # SampleSize    Size of Sample higher than 300, preferable above 500
  # PlotIt        Boolean. TRUE: Plots the challenge
  # PointSize     If PlotIt=TRUE, plotting setting
  # Plotter3D     If PlotIt=TRUE, plotting setting
  #
  # OUTPUT
  # Name    data matrix
  # Cls     numerical vector of classification
  # 
  if(SampleSize<500){
    warning('SampleSize may be to small in order to represent clustering problem correctly.')
  }
  
  if(SampleSize<300){
    SampleSize=300
    warning('SampleSize is to small in order to represent clustering problem correctly. Setting SampleSize=300.')
  }
  
  runif3d=function(...){
    x=runif(...)
    y=runif(...)
    z=runif(...)
    return(t(as.matrix(c(x,y,z))))
  }
  dist2center3d=function(Data){
    d=sqrt((Data[,1]-0)^2+ (Data[,2]-0)^2+(Data[,3]-0)^2)
  }
  runif2d=function(...){
    x=runif(...)
    y=runif(...)
    return(t(as.matrix(c(x,y))))
  }
  dist2center2d=function(Data){
    d=sqrt((Data[,1]-0)^2+ (Data[,2]-0)^2)
  }
  
  runif_ext=function(d=3,...){
    if(d==2) return(runif2d(...))
    if(d==3) return(runif3d(...))
  }
  
  dist2center_ext=function(Data){
    cc=ncol(Data)
    if(cc==2) return(dist2center2d(Data))
    if(cc==3) return(dist2center3d(Data))
  }
  
  PDE_RejectionSamplingPCA=function(DataV,SampleSize=1000){
    Data=DataV$Data
    n=nrow(Data)
    Cls=DataV$Cls
    if(mode(Data)!='numeric'){
      warning('Data is not numeric. Calling "mode(Data)=numeric"')
      mode(Data)='numeric'
    }
    res <- prcomp(x = Data, retx = T, scale = FALSE, tol = 0, 
                  center = FALSE)
    TransData = as.matrix(res$x)
    HighestVariance = TransData[, 1]
    
    kernels=seq(from=min(HighestVariance,na.rm = T),to=max(HighestVariance,na.rm = T),length.out=SampleSize)
    pde=DataVisualizations::ParetoDensityEstimation(HighestVariance,kernels = kernels)
    maxdens=max(pde$paretoDensity)
    
    dens=function(y,pde,maxdens){
      ind=which.min(abs(pde$kernels-y))
      return(pde$paretoDensity[ind]/maxdens)
    }
    sampleind=c()
    i=0
    while(length(sampleind)<SampleSize){
      x=sample(HighestVariance,1)
      y=runif(1,0,1)
      d=dens(x,pde,maxdens)
      if(y<=d){
        i=i+1
        # Works only if all indices are used (not only the first one)
        sampleind=c(sampleind,which(HighestVariance==x))
      }
    }
    takesample=Data[sampleind,]
    if(SampleSize>n){
      Mnull=as.matrix(parallelDist::parDist(Data))
      d=ncol(Data)
      diag(Mnull)=NaN
      x=min(Mnull,na.rm = T)
      y=max(Mnull,na.rm = T)
      Min=1-x/y
      Max=1+x/y
      diff=Max-Min
      M=as.matrix(parallelDist::parDist(takesample))
      M[upper.tri(M)]=NaN
      #dubletten
      ind=which(M==0,arr.ind = TRUE)
      # ind=ind[order(ind[,1]),]
      # print(ind)
      for(i in 1:nrow(ind)){
        v=takesample[ind[i,1],,drop=FALSE]
        w=takesample[ind[i,2],,drop=FALSE]
        if(dist2center_ext(v)>10e-3){
          a <- v * runif_ext(d=d,n=1, min=Min, max=Max)
          
        }else{
          a <- v + runif_ext(d=d,n=1, min=-diff, max=diff)
        }
        if(dist2center_ext(v)>10e-3){
          b <- w * runif_ext(d=d,n=1,  min=Min, max=Max)
          
        }else{
          b <- w + runif_ext(d=d,n=1, min=-diff, max=diff)
        }
        takesample[ind[i,1],]=a
        takesample[ind[i,2],]=b
      }
      
    }
    ClsSample=Cls[sampleind]

    return(list(DataSample=takesample,ClassSample=ClsSample))
  }
  
  switch (Name,
    'Atom' = {
      DataV=FCPS::Atom
    },
    'Chainlink'={
      DataV=FCPS::Chainlink
    },
    'EngyTime'={
      DataV=FCPS::EngyTime
    },
    'GolfBall'={
      DataV=FCPS::GolfBall
    },
    'Hepta'={
      DataV=FCPS::Hepta
    },
    'Lsun3D'={
      DataV=FCPS::Lsun3D
    },
    'Target'={
      DataV=FCPS::Target
    },
    'Tetra'={
      DataV=FCPS::Tetra
    },
    'TwoDiamonds'={
      DataV=FCPS::TwoDiamonds
    },
    'WingNut'={
      DataV=FCPS::WingNut
    },{
      stop('Incorrect Name Selected.')
    }
  )
  DataSample=PDE_RejectionSamplingPCA(DataV,SampleSize = SampleSize)
  # Make sure that outliers are in sample
  if(Name=='Lsun3D'){
    if(length(unique(DataSample$ClassSample))<4){
      ind=sample(1:nrow(DataSample$DataSample),sum(DataV$Cls==4))
      DataSample$ClassSample[ind]=4
      DataSample$DataSample[ind,]=DataV$Data[DataV$Cls==4,]
    }
  }
  
  if(Name=='Target'){
    if(length(unique(DataSample$ClassSample))<6){
      ind=sample(which(DataSample$ClassSample<3),size = sum(DataV$Cls>=3))
      DataSample$ClassSample[ind]=DataV$Cls[DataV$Cls>=3]
      DataSample$DataSample[ind,]=DataV$Data[DataV$Cls>=3,]
    }
  }
  
  if(isTRUE(PlotIt)){
		p=ClusterPlotMDS(DataSample$DataSample,DataSample$ClassSample,main = Name,PointSize=PointSize,Plotter3D=Plotter3D,...)
		p
		if(!is.null(p))    # Plotly
		  print(p)
  }
  names(DataSample)=c(Name,'Cls')
  return(DataSample)
}