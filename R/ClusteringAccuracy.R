ClusteringAccuracy=function(PriorCls,CurrentCls,K=9){
  if(length(unique(PriorCls))>9){
    warning('Too many clusters in PriorCls for RAM of single PC. Please use Cloud Computing, e.g. SparkR')
  }
  #author: 04/2018 MT
  #Note: symmetric ClsToTrueCls() which always works
  NormalizeCls <- function(Cls) {
    #E<-NormalizeCls(Cls);
    #NormalizedCls    <- E$normalizedCls      #    Cls consistently recoded to positive consecutive integers
    #NormalizedClasses<- E$normalizedClasses  #    the different class numbers in NormalizedCls
    #UniqueCls        <- E$uniqueClasses      #    the different class numbers in Cls such that 
    #AnzClasses       <- E$numberOfClasses    #    the number of different classes
    # 
    # Values in Cls are consistently recoded to positive consecutive integers
    # INPUT
    # Cls                  vector of class identifiers can be integers or
    #                      NaN's, need not be consecutive nor positive
    # OUTPUT list of 
    # normalizedCls           Cls consistently recoded to positive consecutive integers
    # normalizedClasses        the different class numbers in NormalizedCls
    # uniqueClasses            the different class numbers in Cls such that 
    #                           NormalizedCls(i) <-> UniqueCls(i)
    # numberOfClasses           the number of different classes
    
    # ALU 2014
    # angepasst an Mdbt und Doku standards
    
    
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
    
    return(
      list(
        normalizedCls = normalizedCls,
        normalizedClasses = normalizedClasses,
        uniqueClasses = uniqueClasses,
        numberOfClasses = numberOfClasses
      )
    )
  }
  ####################################################
  ReduceClassesToK=function(Cls,K=9){
    m=length(unique(Cls))
    ClsRes1=Cls
    if(m>K){
      p=length(seq(from=m,to=K,by=-1))-1
      for(i in 1:p){
        ClsTmp=ClsRes1
        V=ClassCount(ClsTmp)
        ind=order(V$CountPerClass)
        u=head(V$UniqueClasses[ind],2)
        ClsTmp[c(which(ClsTmp==u[1]),which(ClsTmp==u[2]))]=u[1]
        ClsRes1=ClsTmp
      }
    }
    return(ClsRes1)
  }
  ######################################################################
  
  
  standardCls <- NormalizeCls(PriorCls)[[1]]
  givenCls <- NormalizeCls(CurrentCls)[[1]]
  if(length(unique(givenCls))>K){
    warning('Too many clusters in CurrentCls for RAM of single PC. Combining clusters of smalest size.
            Alternativly, please use Cloud Computing, e.g. SparkR')
    givenCls=ReduceClassesToK(givenCls,K=K)
  }

  
  givenCls<- NormalizeCls(givenCls)[[1]]
  
  uniqueClasses <- sort(na.last = T, unique(c(standardCls,givenCls)))
  requireNamespace('pracma')
  allPossiblePermutations <- pracma::perms(uniqueClasses)
  nrOfPermutations <- nrow(allPossiblePermutations)
  nrOfStdClasses <- ncol(allPossiblePermutations)
  givenClasses <- sort(na.last = T, unique(givenCls))
  nrOfGivenClasses <- length(givenClasses)
  renamedCls <- givenCls
  bestAccuracy <- 0
  #For every permutation
  for (i in 1:nrOfPermutations) {
    #set ground truth
    tryRenameCls <- givenCls
    
    #set a permutation of cls to be inspected
    newClassNames <- c(1:nrOfGivenClasses)
    newClassNames[1:nrOfStdClasses] <- allPossiblePermutations[i,]
    
    for (j in 1:nrOfGivenClasses) { #for every point
      #search
      tryRenameCls[which(givenCls == givenClasses[j])] <- newClassNames[j]
    }
    #true positives
    accuracy <- sum(tryRenameCls == standardCls)
    
    if (accuracy > bestAccuracy) {
      renamedCls <- tryRenameCls
      bestAccuracy <- accuracy
    }
  }
  
  bestAccuracy <- bestAccuracy / length(standardCls)
  
  return(Accuracy = bestAccuracy)
}