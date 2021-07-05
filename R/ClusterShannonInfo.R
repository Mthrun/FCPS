ClusterShannonInfo=function(ClsMatrix){
# Info =  ShannonInformation(ClsMatrix) 
  
#  Shannon Information for each column in ClsMatrix, measured in Percent
# INPUT
# ClsMatrix(1:n,1:d)    a matrix of Class vectors , a class vector containing integer values
#
# OUTPUT
# Info(1:d)  = sum(-p * log(p)/MaxInfo) for all unique cases with probability p in ClsMatrix(:,c)
#             for a column with k klasses  MaxInfo = -(1/k)*log(1/k)
# author: ALU, reimplemented from matlab by mct  

#Example
  # data <- as.matrix(iris[,1:4])
  # 
  # # Creating the clusterings for the data set
  # #(here with method complete) for the number of classes 2 to 8
  # hc <- hclust(dist(data), method = "complete")
  #numberOfClusters=100
  # clsm <- matrix(data = 0, nrow = dim(data)[1],  ncol = numberOfClusters-1)
  # for (i in 2:numberOfClusters) {
  #   clsm[,i-1] <- cutree(hc,i)
  # }
  # ClusterShannonInfo(clsm)
 
V = dim(ClsMatrix)
AnzCases=V[1]
AnzVariablen=V[2]

Info=vector(mode = "numeric",length = AnzVariablen)
AnzValues=Info
MaxInfo=Info
MinInfo=Info
MedianInfo=Info
MeanInfo=Info  # INIT

for(c in 1: AnzVariablen){
  V= ClusterCount(ClsMatrix[,c])
  #$UniqueClusters
  #$CountPerCluster
  NumberOfClusters=V$NumberOfClusters
  ClusterPercentages=V$ClusterPercentages


ProbOfClass   = ClusterPercentages/100
InfOfClass    = -ProbOfClass*log(ProbOfClass)
MaxLnInfo     = -(1/NumberOfClusters)*log((1/NumberOfClusters)) # maximale Information bei dieser Anz Auspraegungen

if(MaxLnInfo  >0){
  InfOfClass =  InfOfClass/MaxLnInfo  # Prozentuale Information
}else{
  InfOfClass    = 0 
}
InfOfVariable = InfOfClass
# if(c==1) Info=list(InfOfVariable)
# else Info[c]       = list(InfOfVariable)

if(c==1) Info=InfOfVariable
else Info=DataVisualizations::CombineCols(Info,InfOfVariable)


AnzValues[c]  = NumberOfClusters 
MaxInfo[c]    = max(InfOfClass) 
MinInfo[c]    = min(InfOfClass) 
MedianInfo[c] = median(InfOfClass) 
MeanInfo[c]   = mean(InfOfClass) 
} # fuer alle Variablen
if(!is.null(colnames(ClsMatrix)))
  colnames(Info)=colnames(ClsMatrix)
else
  colnames(Info)=paste0("ClusterNo ",AnzValues)

return(list(Info=Info,ClusterNo=AnzValues,MaxInfo=MaxInfo,MinInfo=MinInfo,MedianInfo=MedianInfo,MeanInfo=MeanInfo))
}