
set.seed(1)
	
library(mgcv)
library(automap,lib="//home/jchang/Scallop_Projects/jchang/R")

load("SandDollarDataAgg.RData")
load("SandDollarPredGrid.RData")
CellSize=500

SpatialPointAllData=SandDollarDataAgg[[1]]
SpatialGridEstimateExtent=SandDollarPredGrid[[1]]

sdgam=gam(ImageDensity~s(Longitude,Latitude,k=15)+s(Depth,k=5),data=SpatialPointAllData@data,family=tw(),method="REML")
sdgam_predvar=predict(sdgam,type="response", se.fit = TRUE)

SpatialPointAllData$GamPredict=sdgam_predvar[[1]]
SpatialPointAllData$GamVar=sdgam_predvar[[2]]
SpatialPointAllData$GamResidual=SpatialPointAllData$ImageDensity-SpatialPointAllData$GamPredict	

sdgam_predvar_grid=predict(sdgam,type="response",newdata=SpatialGridEstimateExtent@data,se.fit=TRUE)

SpatialGridEstimateExtent$GamPredict=sdgam_predvar_grid[[1]]
SpatialGridEstimateExtent$GamVar=sdgam_predvar_grid[[2]]

NMax4LocalKriging=round(dim(SpatialPointAllData)[1]*.5)
AutoKrigeIso=autoKrige(GamResidual~1, SpatialPointAllData, SpatialPointAllData, nmax=NMax4LocalKriging)

SpatialPointAllData$KrigeResidual=AutoKrigeIsoCV$krige.cv_output$var1.pred
SpatialPointAllData$KrigeVar=AutoKrigeIsoCV$krige.cv_output$var1.var
SpatialPointAllData$FinalPredictM2=SpatialPointAllData$GamPredict+SpatialPointAllData$KrigeResidual

AutoKrigeIsoPred=autoKrige(GamResidual~1, SpatialPointAllData, SpatialGridEstimateExtent, block=c(1,1), nmax=NMax4LocalKriging)

SpatialGridEstimateExtent$KrigeResidual=AutoKrigeIsoPred$krige_output$var1.pred
SpatialGridEstimateExtent$KrigeVar=KrigePred$var1.var
SpatialGridEstimateExtent$FinalPredictM2=(SpatialGridEstimateExtent$GamPredict+SpatialGridEstimateExtent$KrigeResidual)
SpatialGridEstimateExtent$FinalPredict=SpatialGridEstimateExtent$FinalPredictM2*CellSize*CellSize



