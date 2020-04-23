TandemClustering=function(Data,ClusterNo,Type="Reduced",PlotIt=FALSE,...){
  
  RenameDescendingClassSize=function (cls) 
  {
    ClassCount=function (Cls) 
    {
      uniqueClasses <- sort(na.last = T, unique(Cls))
      numberOfClasses <- length(uniqueClasses)
      countPerClass <- rep(0, numberOfClasses)
      for (i in 1:numberOfClasses) {
        inClassI <- sum(Cls == uniqueClasses[i])
        countPerClass[i] = inClassI
      }
      classPercentages <- rep(0, numberOfClasses)
      for (i in 1:numberOfClasses) {
        classPercentages[i] <- (countPerClass[i]/sum(countPerClass)) * 
          100
      }
      return(list(UniqueClasses = uniqueClasses, CountPerClass = countPerClass, 
                  NumberOfClasses = numberOfClasses, ClassPercentages = classPercentages, 
                  All2UniqInd = vapply(uniqueClasses, function(i) {
                    max(which(Cls == i))
                  }, 0), Uniq2AllInd = vapply(uniqueClasses, function(i) {
                    which(uniqueClasses == i)
                  }, 0)))
    }
    ListeV <- ClassCount(cls)
    countPerClass <- ListeV[[2]]
    UniqueClasses = ListeV[[1]]
    sortedClasses <- sort(na.last = TRUE, countPerClass, decreasing = TRUE, 
                          index.return = TRUE)
    numberOfClasses <- length(countPerClass)
    renamedCls <- cls
    for (i in 1:numberOfClasses) {
      renamedCls[which(cls == UniqueClasses[sortedClasses$ix[i]], 
                       arr.ind = T)] <- i
    }
    return(renamedCls)
  }

  
  #author: MT, 04/2020
  if(Type!='KernelPCA'){
  d=dim(Data)[2]
  if(d<ClusterNo){
    Cls1=TandemClustering(Data,ClusterNo=2,Type=Type,PlotIt=FALSE,...)$Cls
    cc=length(unique(Cls1))
    while (cc<ClusterNo) {
      ind=which(Cls1==1)
      DataTMP=Data[ind,]
      Cls1tmp=TandemClustering(DataTMP,ClusterNo=2,Type=Type,PlotIt=FALSE,...)$Cls
      NotInd=setdiff(1:nrow(Data),ind)
      Cls1[NotInd]=Cls1[NotInd]+1
      Cls1[ind]=Cls1tmp
      Cls1=RenameDescendingClassSize(Cls1)
      cc=length(unique(Cls1))
    }
    if(isTRUE(PlotIt)){
      if(requireNamespace('DataVisualizations'))
        DataVisualizations::Plot3D(Data,Cls1)
      else
        warning('PlotIT unaavailable because DataVisualizations not installed')
    }
    return(list(Cls=Cls1,Object='Recusively called, because number of dimensions was less than the number of variables.'))
  }
  }else{
    if(!missing(ClusterNo))
      message('TandemClustering of Type KernelPCA does not require "ClusterNo" and will determine the number of clusters automatically.')
  }
  

  switch(Type,
         'Factorial'={
           requireNamespace('clustrd')
           out=clustrd::cluspca(data = Data,nclus = ClusterNo,method = 'FKM',...)
         },
         'Reduced'={
           requireNamespace('clustrd')
           out=clustrd::cluspca(data = Data,nclus = ClusterNo,method = 'RKM',...)},
         'KernelPCA'={
           requireNamespace('kernlab')
           x2=kernlab::kpca(Data,kernel="rbfdot",kpar=list(sigma=3))@rotated
           out=PPCI::ncuth(x2,...) 
         },{
           warning('Incorrect Option Selected')
           return('Incorrect Option Selected')
         }
  )
  #  out=out
  
  Cls=RenameDescendingClassSize(out$cluster)
  
  if(!is.null(rownames(Data)))
    names(Cls)=rownames(Data)
  else
    names(Cls)=1:nrow(Data)
  
  if(isTRUE(PlotIt)){
      ClusterPlotMDS(Data,Cls)
  }
Cls=ClusterRename(Cls,Data)
  return(list(Cls=Cls,Object=out))
}