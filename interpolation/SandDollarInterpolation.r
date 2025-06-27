######################################
#######Example code for doing regression kriging based on Chang et al. (2017; https://doi.org/10.1002/lom3.10174) for sand dollars
#######Jui-Han Chang (jui-han.chang@noaa.gov)
######################################

set.seed(1)
	
library(mgcv)
library(automap)

load("SandDollarDataAgg.RData") #bias-corrected annotation data aggregated every 500 meters based on Chang et al 2017 method
load("SandDollarPredGrid.RData") #prediction grid
CellSize=500 #prediction grid size

SpatialPointAllData=SandDollarDataAgg[[1]] #data were seperated into 5 regions and interpolation was conducted accordingly. here is to run region 1 
SpatialGridEstimateExtent=SandDollarPredGrid[[1]] #prediction grid for region 1

SDGam=gam(ImageDensity~s(Longitude,Latitude,k=30),data=SpatialPointAllData@data,family=tw(),method="REML") #gam model for interpolation
SDGamPredVar=predict(SDGam,type="response", se.fit = TRUE) #using constructed gam model to predict

SpatialPointAllData$GamPredict=SDGamPredVar[[1]] #gam estimates
SpatialPointAllData$GamVar=SDGamPredVar[[2]] #variance of gam estimates
SpatialPointAllData$GamResidual=SpatialPointAllData$ImageDensity-SpatialPointAllData$GamPredict	#gam residuals

SDGamPredVarGrid=predict(SDGam,type="response",newdata=SpatialGridEstimateExtent@data,se.fit=TRUE) #using construced gam to predict on a grid

SpatialGridEstimateExtent$GamPredict=SDGamPredVarGrid[[1]] #gam estimates at grid locations
SpatialGridEstimateExtent$GamVar=SDGamPredVarGrid[[2]] #variance of gam estimates at grid locations

NMax4LocalKriging=round(dim(SpatialPointAllData)[1]*.5) #input parameter for kriging fuction: max distance of data used in kriging
NFoldCV=dim(SpatialPointAllData)[1] #input parameter for kriging fuction: use data length to do leave one out cross validation

AutoKrigeIso=autoKrige(GamResidual~1, SpatialPointAllData, SpatialPointAllData, nmax=NMax4LocalKriging) #kriging on gam residuais
AutoKrigeIsoCV=autoKrige.cv(GamResidual~1, SpatialPointAllData, SpatialPointAllData, miscFitOptions=list(nmax=NMax4LocalKriging,nfold=NFoldCV)) #cross validation of kriging

SpatialPointAllData$KrigeResidual=AutoKrigeIsoCV$krige.cv_output$var1.pred #kriging predicted gam residuals
SpatialPointAllData$KrigeVar=AutoKrigeIsoCV$krige.cv_output$var1.var #kriging variance
SpatialPointAllData$FinalPredictM2=SpatialPointAllData$GamPredict+SpatialPointAllData$KrigeResidual #final interpolation is gam estimates + kriging residuals

AutoKrigeIsoPred=autoKrige(GamResidual~1, SpatialPointAllData, SpatialGridEstimateExtent, block=c(1,1), nmax=NMax4LocalKriging) #kriging model for interpolate at grid locations

SpatialGridEstimateExtent$KrigeResidual=AutoKrigeIsoPred$krige_output$var1.pred #kringing predicted gam residuals at grid locations
SpatialGridEstimateExtent$KrigeVar=AutoKrigeIsoPred$krige_output$var1.var #variance of kriging predicted gam residuals at grid locations
SpatialGridEstimateExtent$FinalPredictM2=(SpatialGridEstimateExtent$GamPredict+SpatialGridEstimateExtent$KrigeResidual) #final estimates per meter squred
SpatialGridEstimateExtent$FinalPredictM2[SpatialGridEstimateExtent$FinalPredictM2<0]=0 #zero the negative predictions
SpatialGridEstimateExtent$FinalPredict=SpatialGridEstimateExtent$FinalPredictM2*CellSize*CellSize #converted the per meter squred estimates to absoluate



