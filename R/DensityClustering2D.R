DensityClustering2D=function(X,Y, MinDensity){
  print('Should someday be reimplemented form ALUs matlab version')
}
# function [DensityCls,Density,Xkernels,Ykernels,F,Fcls,ind] =DensityClusteringXY(X,Y, MinDensity);
# % DensityCls= DensityClusteringXY(X,Y);
# %[DensityCls,Density,Xkernels,Ykernels,F,ind] = DensityClusteringXY(X,Y, MinDensity);
# % Z is the density Value at [X,Y], F is a discrete mash of densisies corresponding to axes Xkernels,Ykernels
# % 
# % INPUT
# % X(1:n),Y(1:n)          a set of 2D points  
# %
# % OPTIONAL
# % MinDensity            the minimum Density>0 , (<1) for clustering, deault: MinDensity = density of Class with less than 5% members
# %                        all points with a lower density are assigned a DensityCls==0;
# % OUTPUT
# % DensityCls(1:n)        a clustering based on esimated Density in [X,Y]
# % Density(1:n)           such that (X,Y,Density) is the smothed density in 3D ; max(Density(:)) ==1
# % Xkernels,Ykernels,F    such that mesh(Xkernels,Ykernels,F) form the smothed densisties for notNANs
# % Fcls                   such that mesh(Xkernels,Ykernels,F,Fcls) gives the assigned classes 
# % ind                    an index such that Z = F(ind); and  DensityCls = Fcls(ind);
# 
# 
# [Density,Xkernels,Ykernels,F,ind] = SmothedDensityXY(X,Y); % Dichte Ausrechnen using Smothed Histograms
# % NaN handling
# [Numbers,XNoNaNInd] = noNaN(X);[Numbers,YNoNaNInd] = noNaN(Y);
# NoNaNInd = intersect(XNoNaNInd,YNoNaNInd);  % index von allen kein NaN in irgendeiner Komponente haben
# X = X(NoNaNInd); Y = Y(NoNaNInd);
# 
# 
# if nargin<3 | length(MinDensity) < 1; % selbststaendige Suche nach MinDensity =  alles unter 5% klassengtoesse weg
# MINPERCENTAGE = 5; 
# MinDensity = 2/100; % default versuch
# LowDensInd = find(F<MinDensity);F(LowDensInd ) =0; CONN=8;
# Fcls  = watershed(-F,CONN);           % give a unique number & calculate watersheds
# Fcls(LowDensInd)=0;                   % die nidrigste wird 0 gesetzt
# Cls = Fcls(ind);                      % DensityCls in XY
# [UniqueClasses,ClassPercentages,CmaxDens] =ClassAnalysisDensityXY(Cls,Density,X,Y); 
# SmallestDensClassInd = min(find(ClassPercentages(2:end)<MINPERCENTAGE)+1);
# if length(SmallestDensClassInd)>0
# MinDensity = max( 0.05,CmaxDens(SmallestDensClassInd)+0.01); % mindestens 5%
#   MinDensity = min( 0.09, MinDensity);                         % aber nicht mehr als 10 %
#   else 
#     MinDensity = 0.05;
# end; % if
# end;
# 
# 
# % jetzt Classen bestimmen mit Watershed transformation NOTE F und ind sind nur auf den NoNaNInd definiert!
#   LowDensInd = find(F<MinDensity);
# F(LowDensInd ) =0;
# CONN=8;
# Fcls  = watershed(-F,CONN);           % give a unique number & calculate watersheds
# Fcls(LowDensInd)=0;                   % die nidrigste wird 0 gesetzt
# DensityCls  = X*0 ;                   % Nan klasse mit 0 initialisiern
# DensityCls(NoNaNInd) = Fcls(ind);               % DensityCls in XY
