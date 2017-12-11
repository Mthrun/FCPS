iheatmap=function(Data=NULL,FestgesetzteClustAnz=0,Directory=getwd()){
#V=iheatmap(Data,FestgesetzteClustAnz=4)
#V=iheatmap(FestgesetzteClustAnz=4) 
#V=iheatmap()
#V=iheatmap(Directory)
#
# INPUT
# OPTIONAL
# Data[d,n]             Der Datensatz, bein NaN werden diese automatisch zu Mittelwerten umgewandelt
# FestgesetzteClustAnz  in soviele Cluster werden die daten eingeteilt, wenn dieser Wert 
#                       fehlt oder =0 gesetzt ist, wird ein Dendrogramm gezeichnet
# Directory             directory where *.lrn file is  (default ==  getwd() )
#  
# Output                Liste V with Cls (if FestgesetzteClustAnz!=0) and Results of heatmap.2
# author: MT 04/16
# requires gplots
# Note: Uses hclust with complete linakge algorithm on euclidean distances
 
  Cls=NULL
  print(Directory)
  setwd(Directory)
requireNamespace('gplots')
  if(is.null(Data)){
    r <- ask2loadFile("lrn")
    if(!is.null(r)){
    FileName <- r$FileName
    FilePath <- r$InDirectory
    
    LrnFile <- ReadLRN(FileName, FilePath)
    Data = LrnFile$Data
    Keys = LrnFile$Key
    if(sum(is.nan(Data))>0){ warning('Data has NaN values. using nantomean().')
      Data=nantomean(Data)
    }
    setwd(FilePath)
   
    #requireRpackage('gplots')
    res=gplots::heatmap.2(Data,scale = "none")
    }else{
      return(-1)
    }
  }else{
#    requireRpackage('gplots')
    if(sum(is.nan(Data))>0){ warning('Data has NaN values. using nantomean().')
      Data=nantomean(Data)
    }
    res=gplots::heatmap.2(Data,scale = "none")
  }
 if(FestgesetzteClustAnz!=0){
   hc <- hclust(dist(Data))
   Cls=cutree(hc,FestgesetzteClustAnz)
 }
  
  return (list(Cls=Cls,ResultsHeatmap2=res))
}