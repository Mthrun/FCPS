ClusterEqualWeighting=function(Cls,Data,MinClusterSize){
# V = ClusterEqualWeighting(Cls)
# V = ClusterEqualWeighting(Cls,Data)
# balance clusters such that their sizes are the same by subsampling the larger cluster
#
# INPUTC
# Cls                  vector of cluster identifiers can be integers or
#                      NaN's, need not be consecutive nor positive
# OPTIONAL
# Data
#
# OUTPUT
# BalancedCls         Vector of Cls such that all classes have the same sizes
# BalancedInd         index such that BalancedCls = Cls(BalancedInd);
#
# BalancedData = Data(BalancedInd,:)
# author: ALU, reimplemented from matlab by mct  
V= ClusterCount(Cls)
UniqueClusters=V$UniqueClusters
CountPerCluster=V$CountPerCluster
NumberOfClusters=V$NumberOfClusters

if(missing(MinClusterSize))
  MinClusterSize = min(CountPerCluster,na.rm = T)

BalancedInd= c()
for(i in 1:NumberOfClusters){
   Current = UniqueClusters[i]                 # die fragliche klasse
   ClusterInd = which(Cls==Current)              # index der Mitgieder in der Klasse
   if(CountPerCluster[i] > MinClusterSize){     # Kuerzung notwendig
    Ind = sample(1:CountPerCluster[i],size = MinClusterSize,replace = F)       # subsample
    ClusterInd = ClusterInd[Ind]         # Kuerzung auf  MinClsAnz
   } else if(CountPerCluster[i] == MinClusterSize){
      Ind = 1:CountPerCluster[i]      # no sample
      ClusterInd = ClusterInd[Ind]         # Kuerzung auf  MinClsAnz
   }else{
      Ind = sample(1:CountPerCluster[i],size = MinClusterSize - CountPerCluster[i],replace = T)       # subsample
      Ind=c(1:CountPerCluster[i],Ind)
      ClusterInd = ClusterInd[Ind]         # Kuerzung auf  MinClsAnz
   }

BalancedInd= c(BalancedInd,ClusterInd)# Aufsammeln des index
}; # for i
BalancedCls = Cls[BalancedInd]

if(!missing(Data)) BalancedData = Data[BalancedInd,] else BalancedData=NULL


return(list(BalancedCls=BalancedCls,BalancedInd=BalancedInd,BalancedData=BalancedData))
}