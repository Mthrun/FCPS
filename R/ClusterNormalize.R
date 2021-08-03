ClusterNormalize <- function(Cls) {
  # Values in Cls are consistently recoded to positive consecutive integers
  
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