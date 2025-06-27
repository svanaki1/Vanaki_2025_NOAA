library(data.table)
library(mgcv)
setwd('/Volumes/T74/')
f1cutoff <- 0.18618
autom <- fread('dollars_paper/autodetections_calibrationset.csv') #calibration set, auto detections
gam6 <- gam(truedetect~s(Conf,Depth,k=7)+s(lat,lon,k=5),data=autom,family=binomial('logit'))
AIC(gam6)
summary(gam6)
plot(gam6)
#plot gam predictions at different depths
confrange <- seq(0.01,1,0.01)
nconf <- length(confrange)
predgam40 <- predict(gam6,type='response',newdata=list(Conf=confrange,Depth=rep(40,nconf),
                                lat=rep(39,nconf),lon=rep(-73.5,nconf)))
predgam50 <- predict(gam6,type='response',newdata=list(Conf=confrange,Depth=rep(50,nconf),
                                          lat=rep(39,nconf),lon=rep(-73.5,nconf)))
predgam60 <-predict(gam6,type='response',newdata=list(Conf=confrange,Depth=rep(60,nconf),
                                          lat=rep(39,nconf),lon=rep(-73.5,nconf)))
predgam70 <- predict(gam6,type='response',newdata=list(Conf=confrange,Depth=rep(70,nconf),
                                          lat=rep(39,nconf),lon=rep(-73.5,nconf)))
plot(predgam40~confrange,type='l',lwd=3,ylim=c(0.03,1),xlab='Confidence',
     ylab='Probability of true detection',xlim=c(0.03,0.97))
lines(predgam50~confrange,lwd=4,col='blue2',lty=3)
lines(predgam60~confrange,lwd=3,col='purple',lty=2)
lines(predgam70~confrange,lwd=3,col='red2',lty=4)
legend('bottomright',c('40 m','50 m','60 m','70 m'),lty=c(1,3,2,4),
       col=c('black','blue2','purple','red2'),title='Depth',lwd=3,cex=1.5)

confthreshold <- 0.025
autom$prob <- predict(gam6,type='response',newdata=list(Conf=autom$Conf,Depth=autom$Depth,
                            lat=autom$lat,lon=autom$lon)) #prob of correct detection
posbyimage <- autom[,.(expnum=sum(prob),ndetect=.N,ndetect2=sum(Conf >= confthreshold,na.rm=TRUE)),
                    by=.(Imagename,Depth,lat,alt)] #expected number of dollars, numdetections
mansum <- fread('dollars_paper/detections_byimage_for_hurdlegams.csv') #calibration set by image
mansum$truepos <- posbyimage$expnum[match(mansum$Imagename,posbyimage$Imagename)] #expected num from auto detections
mansum$truepos[is.na(mansum$truepos)] <- 0
mansum$autodetect <- posbyimage$ndetect2[match(mansum$Imagename,posbyimage$Imagename)] #num detections above threshold
mansum$autodetect[is.na(mansum$autodetect)] <- 0
mansum$presence <- sign(mansum$num) # 1 if at least one dollar, otherwise zero
hurdle15 <- gam(presence~s(truepos,autodetect,k=5)+s(Depth,k=5),
                data=mansum,family=binomial('logit')) #1st stage hurdle
AIC(hurdle15)
summary(hurdle15)
plot(hurdle15)
hurdle2quasi3 <- gam(num~s(truepos)+s(lat,lon,k=5),data=subset(mansum,num>0),
                     family=quasi(link='sqrt',variance='mu^2')) #2nd stage hurdle
summary(hurdle2quasi3)
plot(hurdle2quasi3)
#predict on calibration set
mansum$hurdle1 <- predict(hurdle15,type='response')
mansum$hurdle2 <- predict(hurdle2quasi3,type='response',newdata=list(truepos=mansum$truepos,
                            autodetect=mansum$autodetect,Depth=mansum$Depth,lat=mansum$lat,lon=mansum$lon))
mansum$pred <- mansum$hurdle1*mansum$hurdle2
callm <- lm(pred~num+0,data=mansum)
summary(callm)
par(cex.main=1.75,cex.lab=1.8,cex.axis=1.6, mar=c(5,5,1.5,1.5))
plot(num~pred,data=mansum,xlab='GAM predicted number',cex=1.1,ylim=c(10,500),
     ylab='Number of sand dollars',pch=19)
abline(a=0,b=1,col='red3',lwd=3,lty=2)
sum(mansum$pred) #total predicted number of dollars from GAMS
sum(mansum$num) #total number of dollars
sum(autom$Conf >= f1cutoff) #number predicted by F1 cutoff method
cor(mansum$pred,mansum$num)
#testset
autotest <- fread('dollars_paper/testset_automateddetections.csv')
autotest$prob <- predict(gam6,type='response',newdata=list(Conf=autotest$Conf,Depth=autotest$Depth,lat=autotest$lat,lon=autotest$lon,alt2=autotest$alt2)) 
posbyimagetest <- autotest[,.(expnum=sum(prob),ndetect=sum(Conf>=confthreshold),f1count=sum(Conf > f1cutoff)),by=.(Imagename,Depth,lat)]
testset <- fread('dollars_paper/testset_byimage.csv')
testset$predtp <- posbyimagetest$expnum[match(testset$Imagename,posbyimagetest$Imagename)]
testset$autodetect <- posbyimagetest$ndetect[match(testset$Imagename,posbyimagetest$Imagename)]
testset$f1count <- posbyimagetest$f1count[match(testset$Imagename,posbyimagetest$Imagename)]
testset$hurdle1 <- predict(hurdle15,type='response',newdata=list(autodetect=testset$autodetect,truepos=testset$predtp,
                                                              Depth=testset$Depth,alt=testset$alt,lat=testset$lat,lon=testset$lon))
testset$hurdle1[is.na(testset$hurdle1)] <- 0
testset$hurdle2q <- predict(hurdle2quasi3,type='response',newdata=list(autodetect=testset$autodetect,lat=testset$lat,
                                                                    lon=testset$lon,truepos=testset$predtp,Depth=testset$Depth,alt=testset$alt))
testset$hurdle2q[is.na(testset$hurdle2q)] <- 0
testset$hurdlepredq <- testset$hurdle1*testset$hurdle2q
summary(testset$hurdlepredq)
testlm <- lm(num~hurdlepredq+0,data=testset)
summary(testlm)
cor(testset$num,testset$hurdlepredq)
par(mfrow=c(1,1),cex.main=1.75,cex.lab=1.8,cex.axis=1.6, mar=c(5,5,1.5,1.5))
plot(num~hurdlepredq,data=testset,xlab='GAM predicted number',cex=1.1,ylim=c(10,500),
     ylab='Number of sand dollars',pch=19)
abline(a=0,b=1,col='red3',lwd=3,lty=2)
sum(testset$hurdlepredq)
sum(testset$num)
sum(testset$f1count,na.rm=TRUE)
#predict onto large image set
m10 <- fread('dollars_paper/detections_large_set.csv')
m10$prob <- predict(gam6,type='response',newdata=list(Conf=m10$Conf,Depth=m10$Depth,
                                                      lat=m10$lat,lon=m10$lon)) 
m10$prob[is.na(m10$prob)] <- 0
mab10 <- m10[,.(predtp = sum(prob),ndetect=sum(!is.na(Conf) & Conf > confthreshold),f1detect=sum(!is.na(Conf) & Conf > f1cutoff),
                                           totdetect=sum(!is.na(Conf))),by=.(Imagename,lat,lon,Depth,alt=alt2,fov)]
mab10$predhurdle1 <- predict(hurdle15,type='response',newdata=list(truepos=mab10$predtp,autodetect=mab10$ndetect,
                                                      lat=mab10$lat,lon=mab10$lon,Depth=mab10$Depth))
mab10$predhurdle2 <- predict(hurdle2quasi3,type='response',newdata=list(truepos=mab10$predtp,Depth=mab10$Depth,
                                      autodetect=mab10$ndetect,lat=mab10$lat,lon=mab10$lon))
mab10$pred <- mab10$predhurdle1*mab10$predhurdle2
mab10$density <- mab10$pred/mab10$fov
mab10$dcat <- trunc(mab10$Depth/5)*5
meandensity <- sum(mab10$pred)/sum(mab10$fov)
boxplot((density)^0.25~dcat,data=mab10,pch=19,cex=0.1,ylab='Fourth root of predicted density (1/sqm)',
        xlab='Depth (m)')
#plot of track with estimates
pdf(width=7.8,height=11.4,'dollars_paper/track_plot.pdf')
par(cex.lab=2,cex.axis=1.7, mar=c(4.3,4.4,1.5,1.4))
plot(lat~lon,data=mab10,col='orange2',pch=19,cex=0.3,xlab='Longitude',ylab='Latitude',xaxt='n')
axis(1,at=c(-74:-72))
points(lat~lon,data=subset(mab10,pred>0.5),pch=19,cex=sqrt(density)/18)
plot(st_geometry(coast),add=TRUE)
bathy100 <- subset(bathy,CONTOUR > -150)
plot(st_geometry(bathy100),add=TRUE)
graphics.off()
#fwrite(mab10,'dollars/mab10_final_13june25v2.csv')
