ClusterAccuracy=function(PriorCls,CurrentCls,K=9){
  #
  # INPUT
  # PriorCls      Ground truth,[1:n] numerical vector with n numbers defining the classification.
  #               It has k unique numbers representing the arbitrary labels of the clustering.
  # CurrentCls    Main output of the clustering, [1:n]  numerical vector with n numbers defining the classification.
  #               It has k unique numbers representing the arbitrary labels of the clustering.              
  # K             Maximal number of classes for computation. Default K=9.
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
      
      uniqueClasses <- sort(na.last = T, unique(Cls))
      numberOfClasses <- length(uniqueClasses)
      unique2Cls <- NULL #  initializing the vector
      
      for (i in 1:length(Cls)) {
        # calculating the indexes of elements of Cls in uniqueClasses
        unique2Cls <- c(unique2Cls, which(uniqueClasses == Cls[i]))
      }
      
      if (numberOfClasses > 0) {
        normalizedClasses <- c(1:numberOfClasses)
        normalizedCls <- normalizedClasses[unique2Cls]
      }
      else {
        normalizedClasses <- Cls
      }
      
      return(normalizedCls)
    }
    ####################################################
    
    ReduceClassesToK=function(Cls,K=9){
      m=length(unique(Cls))
      ClsRes1=Cls
      if(m>K){
        p=length(seq(from=m,to=K,by=-1))-1
        for(i in 1:p){
          ClsTmp=ClsRes1
          V=ClusterCount(ClsTmp)
          ind=order(V$CountPerClass)
          u=head(V$UniqueClasses[ind],2)
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
      givenCls=ReduceClassesToK(givenCls,K=K)
    }
    givenCls<- NormalizeCls(givenCls)
    
    uniqueClasses <- sort(na.last = T, unique(c(standardCls,givenCls)))
    requireNamespace('pracma')
    allPossiblePermutations <- pracma::perms(uniqueClasses)
    nrOfPermutations <- nrow(allPossiblePermutations)
    nrOfStdClasses <- ncol(allPossiblePermutations)
    givenClasses <- sort(na.last = T, unique(givenCls))
    nrOfGivenClasses <- length(givenClasses)
    renamedCls <- givenCls
    bestAccuracy <- 0
    # For every permutation
    for (i in 1:nrOfPermutations) {
      # Set ground truth
      tryRenameCls <- givenCls
      
      # Set a permutation of cls to be inspected
      newClassNames <- c(1:nrOfGivenClasses)
      newClassNames[1:nrOfStdClasses] <- allPossiblePermutations[i,]
      
      for (j in 1:nrOfGivenClasses) { # For every point
        # Search
        tryRenameCls[which(givenCls == givenClasses[j])] <- newClassNames[j]
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
