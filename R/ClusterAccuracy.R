ClusterAccuracy=function(PriorCls,CurrentCls,K=9){
  #
  # INPUT
  # PriorCls      Ground truth,[1:n] numerical vector with n numbers defining the classification.
  #               It has k unique numbers representing the arbitrary labels of the clustering.
  # CurrentCls    Main output of the clustering, [1:n]  numerical vector with n numbers defining the classification.
  #               It has k unique numbers representing the arbitrary labels of the clustering.              
  # K             Maximal number of Labels for computation. Default K=9.
  # 
  # OUTPUT
  # Accuracy      Number
  # 
  # Author: 04/2018 MT
  PriorCls[!is.finite(PriorCls)]=9999
  CurrentCls[!is.finite(CurrentCls)]=9999
  
    if(length(unique(PriorCls))>9){
      warning('Too many clusters in PriorCls for RAM of single PC. Please use cloud computing, e.g. SparkR')
    }
    
    #Note: symmetric ClsToTrueCls() which always works
    
    NormalizeCls <- function(Cls) {# Values in Cls are consistently recoded to positive consecutive integers
      
      uniqueLabels <- sort(na.last = T, unique(Cls))
      numberOfLabels <- length(uniqueLabels)
      unique2Cls <- NULL #  initializing the vector
      
      for (i in 1:length(Cls)) {
        # calculating the indexes of elements of Cls in uniqueLabels
        unique2Cls <- c(unique2Cls, which(uniqueLabels == Cls[i]))
      }
      
      if (numberOfLabels > 0) {
        normalizedLabels <- c(1:numberOfLabels)
        normalizedCls <- normalizedLabels[unique2Cls]
      }
      else {
        normalizedLabels <- Cls
      }
      
      return(normalizedCls)
    }
    ####################################################
    
    ReduceClsToK=function(Cls,K=9){
      m=length(unique(Cls))
      ClsRes1=Cls
      if(m>K){
        p=length(seq(from=m,to=K,by=-1))-1
        for(i in 1:p){
          ClsTmp=ClsRes1
          V=ClusterCount(ClsTmp)
          ind=order(V$CountPerCluster)
          u=head(V$UniqueClusters[ind],2)
          ClsTmp[c(which(ClsTmp==u[1]),which(ClsTmp==u[2]))]=u[1]
          ClsRes1=ClsTmp
        }
      }
      return(ClsRes1)
    }
    ######################################################################
    
    
    standardCls <- NormalizeCls(PriorCls)
    givenCls <- NormalizeCls(CurrentCls)
    if(length(unique(givenCls))>K){
      warning('Too many clusters in CurrentCls for RAM of single PC. Combining clusters of smallest size.
            Alternatively, please use cloud Computing, e.g. SparkR')
      givenCls=ReduceClsToK(givenCls,K=K)
    }
    givenCls<- NormalizeCls(givenCls)
    
    uniqueLabels <- sort(na.last = T, unique(c(standardCls,givenCls)))
    requireNamespace('pracma')
    allPossiblePermutations <- pracma::perms(uniqueLabels)
    nrOfPermutations <- nrow(allPossiblePermutations)
    nrOfStdLabels <- ncol(allPossiblePermutations)
    givenLabels <- sort(na.last = T, unique(givenCls))
    nrOfGivenLabels <- length(givenLabels)
    renamedCls <- givenCls
    bestAccuracy <- 0
    # For every permutation
    for (i in 1:nrOfPermutations) {
      # Set ground truth
      tryRenameCls <- givenCls
      
      # Set a permutation of cls to be inspected
      newClassNames <- c(1:nrOfGivenLabels)
      newClassNames[1:nrOfStdLabels] <- allPossiblePermutations[i,]
      
      for (j in 1:nrOfGivenLabels) { # For every point
        # Search
        tryRenameCls[which(givenCls == givenLabels[j])] <- newClassNames[j]
      }
      # True positives
      accuracy <- sum(tryRenameCls == standardCls)
      
      if (accuracy > bestAccuracy) {
        renamedCls <- tryRenameCls
        bestAccuracy <- accuracy
      }
    }
    
    bestAccuracy <- bestAccuracy / length(standardCls)
    
    return(Accuracy = bestAccuracy)
}
