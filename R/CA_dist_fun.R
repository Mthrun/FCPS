CA_dist_fun=function(i,fun,Distances,ClusterNo,SetSeed=TRUE,...){

  if(isTRUE(SetSeed)){
    seedno=1000+i
    set.seed(seed = seedno)
    nndelta=paste0('Seed_',seedno)
  }else{
    nndelta=paste0(i)
    set.seed(seed = NULL)
  }
  prior=Sys.time()
  string=names(formals(fun))
  
  if(is.null(ClusterNo)){
      object=R.utils::doCall(fun, args=list(DataOrDistances=Distances,...),.ignoreUnusedArgs=TRUE)
  }else{
      object=R.utils::doCall(fun,  args=list(DataOrDistances=Distances,ClusterNo=ClusterNo,...),.ignoreUnusedArgs=TRUE)
  }
  past=Sys.time()
  delta=as.vector(as.numeric(difftime(past,prior,units = 'secs')))
  names(delta)=nndelta
  nn=names(object)
  ind=which(nn=='Cls')
  if(length(ind)==1){
    Liste=list(Cls=object[[ind]],ComputationTime=delta,Seed=seedno,CAs=object)
  }else{
    warning('"Cls" object could not be found. Everything available is returned.')
    Liste=list(Cls=NULL,ComputationTime=delta,Seed=seedno,CAs=object)
  }
  return(Liste)
}#end help_fun