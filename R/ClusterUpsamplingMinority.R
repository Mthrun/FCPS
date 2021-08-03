ClusterUpsamplingMinority=function(Cls,Data,MinorityCluster,Percentage=200,knn=5,PlotIt=FALSE){
  
  if(Percentage<100){
    warning("ClusterUpsamplingMinority: Percentage below 100, returning NULL")
    return(list(ClsExt=NULL,DataExt=NULL))
  }
  ind=which(Cls==MinorityCluster)
  internaldata=cbind(Data,Cls)
  if(length(ind)>0){
    out=smote.exs(data = internaldata[ind,],tgt = ncol(internaldata),N = Percentage,k = knn)
    upsampleddata=out[,1:(ncol(internaldata)-1)]
    DataNew=rbind(Data,upsampleddata)
    ClsNew=c(Cls,rep(MinorityCluster,nrow(upsampleddata)))
    
    if(isTRUE(PlotIt))
      ClusterPlotMDS(DataOrDistances = DataNew,Cls = ClsNew,main = paste0("Cluster ",MinorityCluster," up sampled by SMOTE algorithm."))
    
    return(list(ClsExt=ClsNew,DataExt=DataNew))
    
  }else{
    warning("ClusterUpsamplingMinority: MinorityCluster not found. Returning NULL.")
    return(list(ClsExt=NULL,DataExt=NULL))
  }

 }


smote.exs <- function(data,tgt,N,k)
  # INPUTS:
  # data are the rare cases (the minority "class" cases)
  # tgt is the name of the target variable
  # N is the percentage of over-sampling to carry out;
  # and k is the number of nearest neighours to use for the generation
  # OUTPUTS:
  # The result of the function is a (N/100)*T set of generated
  # examples with rare values on the target
  #author: L. Torgo, Feb 2010
{
  nomatr <- c()
  T <- matrix(nrow=dim(data)[1],ncol=dim(data)[2]-1)
  for(col in seq.int(dim(T)[2]))
    if (class(data[,col]) %in% c('factor','character')) {
      T[,col] <- as.integer(data[,col])
      nomatr <- c(nomatr,col)
    } else T[,col] <- data[,col]
  
  if (N < 100) { # only a percentage of the T cases will be SMOTEd
    nT <- NROW(T)
    idx <- sample(1:nT,as.integer((N/100)*nT))
    T <- T[idx,]
    N <- 100
  }
  
  p <- dim(T)[2]
  nT <- dim(T)[1]
  
  ranges <- apply(T,2,max)-apply(T,2,min)
  
  nexs <-  as.integer(N/100) # this is the number of artificial exs generated
  # for each member of T
  new <- matrix(nrow=nexs*nT,ncol=p)    # the new cases
  
  for(i in 1:nT) {
    
    # the k NNs of case T[i,]
    xd <- base::scale(T,T[i,],ranges)
    for(a in nomatr) xd[,a] <- xd[,a]==0
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    kNNs <- order(dd)[2:(k+1)]
    
    for(n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k,1)
      
      ex <- vector(length=ncol(T))
      
      # the attribute values of the generated case
      difs <- T[kNNs[neig],]-T[i,]
      new[(i-1)*nexs+n,] <- T[i,]+runif(1)*difs
      for(a in nomatr)
        new[(i-1)*nexs+n,a] <- c(T[kNNs[neig],a],T[i,a])[1+round(runif(1),0)]
      
    }
  }
  newCases <- data.frame(new)
  for(a in nomatr)
    newCases[,a] <- factor(newCases[,a],levels=1:nlevels(data[,a]),labels=levels(data[,a]))
  
  newCases[,tgt] <- factor(rep(data[1,tgt],nrow(newCases)),levels=levels(data[,tgt]))
  colnames(newCases) <- colnames(data)
  newCases
}