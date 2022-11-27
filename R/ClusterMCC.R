ClusterMCC=function(PriorCls,CurrentCls,Force=TRUE){
 
  if (!requireNamespace('yardstick',quietly = TRUE)) {
    message(
      'Subordinate  package (yardstick) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      MCC = "Subordinate package (yardstick) is missing.
                Please install the package which is defined in 'Suggests'."
      
    )
  }
  
  PriorCls[!is.finite(PriorCls)]=9999
  CurrentCls[!is.finite(CurrentCls)]=9999
  
  if(isTRUE(Force)){
    u1=sort(unique(PriorCls))
    u2=sort(unique(CurrentCls))
    s1=setdiff(u1,u2)
    s2=setdiff(u2,u1)
    ss=unique(c(s1,s2))
    if(length(ss)>0){
      message("ClusterMCC: No.of Clusters in PriorCls does not equal CurrentCls. Adding missing unique label(s).")
      PriorCls=c(PriorCls,ss)
      CurrentCls=c(CurrentCls,ss)
    }
  }
  CurrentClsFactor=as.factor(CurrentCls)
  PriorClsFactor=as.factor(PriorCls)
  if(length(levels(CurrentClsFactor))!=length(levels(PriorClsFactor))){
    message("ClusterMCC: No.of Clusters in PriorCls does not equal CurrentCls. Please make sure that the mapping of numbers in ground truth is equal to to mapping of the clustering.")
    if(length(levels(CurrentClsFactor))<length(levels(PriorClsFactor))){
      levels(CurrentClsFactor)=levels(PriorClsFactor)
    }else{
      levels(PriorClsFactor)=levels(CurrentClsFactor)
    }
      
  }

  mcc=yardstick::mcc_vec(truth = PriorClsFactor,estimate = CurrentClsFactor)
  
  return(MCC = mcc)
}
