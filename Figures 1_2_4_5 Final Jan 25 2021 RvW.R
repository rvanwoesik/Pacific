
# FIGURE 1
library(plyr)
#### 
setwd("E:/ACCRETION OF REEFS/Accretion model 2018/Compare carbonate and SST/G an LCC")
allpts<-read.csv('islands data.csv')
islsdat <- ddply(allpts, c('site',"Island",'P.locat'), summarise,
                 LCC    = mean(LCC),
                 lat = mean(lat),
                 lon = mean(lon),
                 GP = mean (GP),
                 NP = mean (NP))
                 #orientation=mean(orientation)            )


head(islsdat)
islsdat


Table1<-read.csv('E://ACCRETION OF REEFS/figures 2020/islands dataRug.csv')
Table2<-ddply(Table1,c("Island",'site','P.locat'),summarize,
              meanLCC    = mean(LCC),
              sdLCC = sd(LCC),
              latitude = mean(lat),
              longitude = mean(lon),
              meanGrossProduction = mean (GP),
              sdGrossProduction = sd (GP),
              meanNetProduction = mean (NP),
              sdNetProduction =sd(NP),
              meanBioerosion = mean(BFj+BUj),
              sdBioerosion=sd(BFj+BUj),
              meanRugosity=mean(Rugosity),
              sdRugosity=sd(Rugosity))
setwd("E:/ACCRETION OF REEFS/Figures and manuscript Dec2020")
write.csv(Table2,"DataTable.csv")




library(rgdal)

islsdat$lon
#Need to add 360 to all negative longitudes
lonn <- vector(length=nrow(islsdat))
for (i in 1:nrow(islsdat)) {
  if (islsdat[i,]$lon < 0) {
    lonn[i] <- islsdat[i,]$lon + 360
  } else {
    lonn[i] <- islsdat[i,]$lon
  }
}
lonn

islsdat$lon= lonn
coordinates(islsdat)<-~lon+lat

library(raster)
head(islsdat)
fig1ras<-raster("C:/Users/ccacc/Desktop/van Woesik/Pohnpei Kosrae/sst CORTAD data/Figure 1 mean TSA frequency 2000-2020.nc")

plot(fig1ras)
library(maptools)
data("wrld_simpl")
plot(wrld_simpl,add=T)
points(islsdat)

compassRose<-function(x,y,rot=0,cex=1,cex.dir=1,llwd=1,col='black') { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints,lwd=llwd,border=col) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S"),cex=cex.dir,col=col) 
  par(oldcex) 
}



library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)



setwd("E:/ACCRETION OF REEFS/figures 2020")

tiff(paste('Figure 1 TSA freq.png'),width=3900,height=1500, res = 300)

par(mar=c(1,1,1,1))
image(fig1ras/52,maxpixels=500000,col=colorRampPalette((c('blue','turquoise2', 'green','yellow','orange','red')))(200),zlim=c(0,10),axes = F,ylab='',xlab='')
#,mar=c(2,5,2,0)
box(lwd=1.5)
plot(world$geometry,add=T,col='burlywood')
#plot.map("world", transf=F, center=210 , col="burlywood",bg="white",ylim=c(-45,45),fill=TRUE,mar=c(2,5,2,0),add=F) #center is still 0
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="lightsteelblue1")
#plot.map("world", transf=F,center=210 , col="burlywood",bg="white",ylim=c(-45,45),fill=TRUE,mar=c(2,5,2,0),add=T,xlab='longitude',ylab='latitude',lwd=1.5) #center is still 0


compassRose(190,-5,cex=.75,cex.dir=1.2,llwd=1.5)

axis(1,at=c(140,160,180,200),labels=c("",'','',''), tck = .025,mgp=c(3,.3,0),cex.axis=.975,lwd=1.5)

mtext(parse(text='160^o*E'), 1, line=-1.5, adj=0.385,cex=.975)
mtext(parse(text='160^o*W'),1,line=-1.5,adj=0.886,cex=.975)

axis(2,at=c(-5,0,5,10),labels=c('','','',''), tck = .025,mgp=c(3,.405,0),cex.axis=.975,lwd=1.5)

mtext(parse(text="5^o*S"), 2, line=-1.5, adj=0.179,cex=.975)
mtext(parse(text='5^o*N'),2,line=-1.5,adj=0.62,cex=.975)

#image(mean(get(paste('RTDone',scenario,sep=''))),maxpixels=3836160,add=T,col = c(color.palette2,color.palette1),breaks=palette.breaks)
library(plotrix)
color.legend(190,8,200,9,legend=c(0,5,10), rect.col=colorRampPalette((c('blue','turquoise2', 'green','yellow','orange','red')))(200),cex=.975)

mtext((expression("Yearly frequency of SST >" * 1^o * "C above climatology")), 1, line=-16.05, adj=0.91,cex=.975)

text(mean(islsdat[islsdat$Island=='Palau',]$lon),mean(islsdat[islsdat$Island=='Palau',]$lat+.8),'Palau',cex=1.2)
text(mean(islsdat[islsdat$Island=='Yap',]$lon),mean(islsdat[islsdat$Island=='Yap',]$lat+.8),'Yap',cex=1.2)
text(mean(islsdat[islsdat$Island=='Pohnpei',]$lon),mean(islsdat[islsdat$Island=='Pohnpei',]$lat+.8),'Pohnpei',cex=1.2)
text(mean(islsdat[islsdat$Island=='Kosrae',]$lon),mean(islsdat[islsdat$Island=='Kosrae',]$lat+.8),'Kosrae',cex=1.2)
text(mean(islsdat[islsdat$Island=='Majuro',]$lon),mean(islsdat[islsdat$Island=='Majuro',]$lat+.8),'Majuro',cex=1.2)
text(mean(islsdat[islsdat$Island=='Kiritimati',]$lon),mean(islsdat[islsdat$Island=='Kiritimati',]$lat+.8),'Kiritimati',cex=1.2)

points(mean(islsdat[islsdat$Island=='Palau',]$lon),mean(islsdat[islsdat$Island=='Palau',]$lat),pch=16)
points(mean(islsdat[islsdat$Island=='Yap',]$lon),mean(islsdat[islsdat$Island=='Yap',]$lat),pch=16)
points(mean(islsdat[islsdat$Island=='Pohnpei',]$lon),mean(islsdat[islsdat$Island=='Pohnpei',]$lat),pch=16)
points(mean(islsdat[islsdat$Island=='Kosrae',]$lon),mean(islsdat[islsdat$Island=='Kosrae',]$lat),pch=16)
points(mean(islsdat[islsdat$Island=='Majuro',]$lon),mean(islsdat[islsdat$Island=='Majuro',]$lat),pch=16)
points(mean(islsdat[islsdat$Island=='Kiritimati',]$lon),mean(islsdat[islsdat$Island=='Kiritimati',]$lat),pch=16)


#text(-72.31,-25.336,'Indian Ocean',cex=1.2)
text(180,11,'Pacific Ocean',cex=1.2)
scalebar(d=9,xy=c(170,-7),label=c(0,'',1000),cex=.9,type='bar',divs=4,below="kilometers",adj=c(0.5,-1.1))
dev.off()













#### Figure 2 Time series 

#### 
setwd("E:/ACCRETION OF REEFS/Accretion model 2018/Compare carbonate and SST/G an LCC")
allpts<-read.csv('islands data.csv')
islsdat <- ddply(allpts, c('site',"Island",'P.locat'), summarise,
                 LCC    = mean(LCC),
                 lat = mean(lat),
                 lon = mean(lon),
                 GP = mean (GP),
                 NP = mean (NP))
#orientation=mean(orientation)            )


head(islsdat)
islsdat

Fig2ras<-TSA<-brick("C:/Users/ccacc/Desktop/van Woesik/Pohnpei Kosrae/sst CORTAD data/cortadv6_TSA.nc",varname="TSA_DHW")
pal<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Palau'],islsdat$lat[islsdat$Island=='Palau']))
palmean1<-colMeans(pal)

Yap<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Yap'],islsdat$lat[islsdat$Island=='Yap']))
yapmean1<-colMeans(Yap,na.rm=T)

kos<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Kosrae'],islsdat$lat[islsdat$Island=='Kosrae']))
kosmean1<-colMeans(kos,na.rm=T)

pohn<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Pohnpei'],islsdat$lat[islsdat$Island=='Pohnpei']))
pohnmean1<-colMeans(pohn,na.rm=T)

maj<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Majuro'],islsdat$lat[islsdat$Island=='Majuro']))
majmean1<-colMeans(maj,na.rm=T)

kir<-raster::extract(Fig2ras,cbind(islsdat$lon[islsdat$Island=='Kiritimati'],islsdat$lat[islsdat$Island=='Kiritimati']))
kirmean1<-colMeans(kir,na.rm=T)

#palmean<-palmean1
palmean2<-data.frame(palmean1)
yapmean2<-data.frame(yapmean1)
kosmean2<-data.frame(kosmean1)
pohnmean2<-data.frame(pohnmean1)
majmean2<-data.frame(majmean1)
kirmean2<-data.frame(kirmean1)



library(lubridate)
library(timeSeries)
dates<-names(palmean1)
dates<-sub('.','',c(dates))
dates<-ymd(dates)

names(palmean1)<-dates
palmeants<-ts(palmean1,start=1,frequency=7)
paldecomp<-decompose(palmean1)

year<-substr(names(palmean1), start = 1, stop = 4)
palmean2$year<-year

aggpal <- aggregate(palmean2$palmean1, by=list(palmean2$year), max)
aggyap <- aggregate(yapmean2$yapmean1, by=list(palmean2$year), max)
aggkos <- aggregate(kosmean2$kosmean1, by=list(palmean2$year), max)
aggpohn <- aggregate(pohnmean2$pohnmean1, by=list(palmean2$year), max)
aggmaj <- aggregate(majmean2$majmean1, by=list(palmean2$year), max)
aggkir <- aggregate(kirmean2$kirmean1, by=list(palmean2$year), max)



library(pracma)
moven<-10
movtyp<-'m'

setwd("E:/ACCRETION OF REEFS/figures 2020")
tiff(paste('Figure 2 time series.png'),width=2500,height=1500, res = 300)

plot(dates,movavg(palmean2$palmean1,moven,movtyp),type='l',col='blue',ylim=c(0,20),ylab='Degrees heating weeks',xlab='Years')

#abline(v=10228,lty=2)
#text(10035.52,13,'1998 El Nino',srt=90)
#abline(v=16439.91,lty=2)
#text(16255.52,13,'2015 El Nino',srt=90)
lwdcontrl<-2
abline(v=dates[57],lty=2,col='grey',lwd=lwdcontrl*2.3) #83
abline(v=dates[275],lty=2,col='grey',lwd=lwdcontrl*1.6) #87
abline(v=dates[525],lty=2,col='grey',lwd=lwdcontrl*1.8) #
abline(v=dates[596],lty=2,col='grey',lwd=lwdcontrl*.9)
abline(v=dates[677],lty=2,col='grey',lwd=lwdcontrl*1.1)
abline(v=dates[833],lty=2,col='grey',lwd=lwdcontrl*2.4)#98
abline(v=dates[1097],lty=2,col='grey',lwd=lwdcontrl*1.5)
abline(v=dates[1197],lty=2,col='grey',lwd=lwdcontrl*.9)
abline(v=dates[1302],lty=2,col='grey',lwd=lwdcontrl*1)
abline(v=dates[1462],lty=2,col='grey',lwd=lwdcontrl*1.5)
abline(v=dates[1775],lty=2,col='grey',lwd=lwdcontrl*2.5)
abline(v=dates[1929],lty=2,col='grey',lwd=lwdcontrl*1.2)


lines(dates,movavg(palmean2$palmean1,moven,movtyp),type='l',col='blue')
lines(dates,movavg(yapmean2$yapmean1,moven,movtyp),type='l',col='purple')
lines(dates,movavg(pohnmean2$pohnmean1,moven,movtyp),type='l',col='green')
lines(dates,movavg(kosmean2$kosmean1,moven,movtyp),type='l',col='yellow')
lines(dates,movavg(majmean2$majmean1,moven,movtyp),type='l',col='orange')
lines(dates,movavg(kirmean2$kirmean1,moven,movtyp),type='l',col='red')
legend('topleft',c('Palau','Yap','Pohnpei','Kosrae','Majuro','Kiritimati'),lty=1,col=c('blue','purple','green','yellow','orange','red'))

box()
dev.off()



plot(dates,palmeants,type='l')
plot(aggpal[,1],aggpal[,2],type='l',col='blue',ylim=c(0,25))
lines(aggpal[,1],aggyap[,2],type='l',col='purple')
lines(aggpal[,1],aggkos[,2],type='l',col='green')
lines(aggpal[,1],aggpohn[,2],type='l',col='yellow')
lines(aggpal[,1],aggmaj[,2],type='l',col='orange')
lines(aggpal[,1],aggkir[,2],type='l',col='red')














#### Figure 4 Jags threshold...


setwd("E:/Jags")
###################################################################
#Set the working directory
#setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GAMM/Data/ReefData")
#source the file: HighstatLibV6.R
source(file="HighstatLibV6.R")  
source("SupportFilesHighStat.R")
source("MCMCSupportHighstatV2.R")
##################################################################
# CO2 <- read.table("CoralData.txt",
#                  header = TRUE)
# str(CO2)
# names(CO2)
library(readr)
setwd("E:/ACCRETION OF REEFS/Accretion model 2018/Compare carbonate and SST/G an LCC")
CO<- read_csv('islands data.csv')
CO<-CO[,-1]
str(CO)
names(CO)[3]<-'Net_production'
names(CO)[2]<-'reef'
names(CO)[1]<-'Country'


#CO[,10]<-CO$Accretion_rate-0.06;names(CO)[10]<-'Net_production' #remove sedimentation from accretion
CO[,11]<-CO$BFj+CO$BUj+0.009664352;names(CO)[11]<-'Gross_erosion'

CO$LCC<-CO$LCC/10 #convert from cm/10m to percentage

CO$reef<-paste(CO$Country,CO$reef)
names(CO)[9]<-'Site'
CO$Site<-1:852
#CO$reef[CO$reef=='outer']<-'P-outer'
#CO$reef[CO$reef=='patch']<-'P-patch'
#CO$reef[CO$reef=='inner']<-'P-inner'

# CO1<- read_csv("Kosrae site model ts2.csv")
# CO1<-CO1[,-1]
# names(CO1)<-c('reef','Accretion_rate','Gross_production','BFj','BUj',"palsites.lon","palsites.lat","Site",'LCC')
# CO1[,10]<-CO1$Accretion_rate-0.06;names(CO1)[10]<-'Net_production' #remove sedimentation from accretion
# CO1[,11]<-CO1$BFj+CO1$BUj+0.009664352;names(CO1)[11]<-'Gross_erosion'
# CO1$LCC<-CO1$LCC/10 #convert from cm/10m to percentage
# CO1$Site<-CO1$Site+24
# CO1$reef[CO1$reef=='outer']<-'K-outer'
# CO1$reef[CO1$reef=='patch']<-'K-patch'
# CO1$reef[CO1$reef=='inner']<-'K-inner'
# 
# CO2<-rbind(CO,CO1)
# CO<-cbind(c(rep("Pohnpei",144),rep('Kosrae',144)),CO2);names(CO)[1]<-'Country'
###################################################################

#Data taken from:
#Perry CT, Murphy GN, Kench PS, Smithers SG, Edinger EN, Steneck RS,
#Mumby PJ (2013) Caribbean-wide decline in carbonate production 
#threatens coral reef growth.
#Nature Communications 4: 1402, doi:10.1038/ncomms2409





###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(R2jags)
library(nlme)
library(mgcv)



##################################################################
#Housekeeping
CO$fCountry <- factor(CO$Country)
CO$fSite    <- factor(CO$Site)
CO$fHabitat <- factor(CO$reef) #switch to outer inner whatnot
CO$G        <- CO$Net_production
##################################################################



##################################################################
#Data exploration
MyVar <- c("G", "LCC")
Mydotplot(CO[,MyVar])

table(CO$fCountry)
table(CO$fSite)
table(CO$fHabitat)
##################################################################




##################################################################
#Figure 3.2
split.screen(c(2,1))                 # split display into two screens
split.screen(c(1,2), screen = 1)     # split top half in two

screen(2)
par(cex.lab = 1.5, mar = c(5,3,2,2))
boxplot(G ~ fSite, data = CO, xlab = "Site")

screen(3)
par(cex.lab = 1.5, mar = c(5,3,2,2))
boxplot(G ~ fCountry, data = CO, xlab = "Country")

screen(4)
par(cex.lab = 1.5, mar = c(5,3,2,2))
boxplot(G ~ fHabitat, data = CO, xlab = "Habitat")

dev.off()
#Figure 3.3	
MyYlab <- expression(paste("Net carbonate production ", 
                           "(kg CaCO"[3],
                           " m"^"-2",
                           "year" ^"-1",")"))
par(mar = c(5,6,2,2), cex.lab = 1.5)
plot(x = CO$LCC, 
     y = CO$G,
     pch = 16,
     cex = 1,
     xlab = "LCC (%)",
     ylab = MyYlab)
abline(h = 0, lty = 2)
##################################################################



##################################################################
#Figure 3.4

#setwd("E:/ACCRETION OF REEFS/Accretion model 2018/Accretion Pohnpei Kosrae 2018/Figures")
#tiff('Carb production Jags.png',width=1400*sz,height=800*sz, res = 300)

split.screen(c(2,1))        # split display into two screens
split.screen(c(1,2), screen = 1)     # split top half in two

screen(2)
par(cex.lab = 1.1, mar = c(5,5,3,2))
boxplot(G ~ fSite, data = CO, xlab = "Site", ylab = (expression(paste(Carbonate~production~(kg~m^2~y^-1)))),cex.lab=1)

screen(3)
par(cex.lab = 1.1, mar = c(5,5,3,2))
boxplot(G ~ fCountry, data = CO, xlab = "Country", ylab = (expression(paste(Carbonate~production~(kg~m^2~y^-1)))),cex.lab=1)

screen(4)
par(cex.lab = 1.1, mar = c(5,5,3,2))
boxplot(G ~ fHabitat, data = CO, xlab = "Habitat", ylab = (expression(paste(Carbonate~production~(kg~m^2~y^-1)))),cex.lab=1)
##################################################################

dev.off()



setwd("E:/ACCRETION OF REEFS/figures 2020")
tiff('islands carb Production.png',width=2000,height=1000, res = 300)

CO$fCountry <- factor(CO$fCountry , levels=c("Palau", "Yap", "Pohnpei", "Kosrae",'Majuro','Kiritimati'))

par(cex.lab = 1.1, mar = c(5,5,3,2))
boxplot(G ~ fCountry, data = CO, xlab = "Country", ylab = (expression(paste(Carbonate~production~(kg~m^2~y^-1)))),cex.lab=1)

dev.off()














# ########################################################################
# #3.7 MCMC and Gaussian additive mixed effects models
# 
# 
# #Covariate matrix
# Xcov <- model.matrix(~ 1 + fHabitat, data = CO)    
# K <- ncol(Xcov)
# 
# 
# #Center covariates
# CO$LCC.std <- Mystd(CO$LCC)
# 
# 
# #Use O'Sullivan splines
# #Get X and Z matrices for LCC.std smoother
# #Smoother: X * b + Z * u
# numIntKnots <- 5
# intKnots <- quantile(unique(CO$LCC.std),
#                      seq(0,1,length=(numIntKnots+2))[-c(1,(numIntKnots+2))])
# 
# XZ <- OSullivan(CO$LCC.std, 
#                 numIntKnots = 5, 
#                 AddIntercept = FALSE,
#                 intKnots = intKnots)
# 
# #Random effect Site
# re  <- as.numeric(as.factor(CO$Site))
# Nre <- length(unique(re))
# 
# #Get all the data fit JAGS
# #This is in fact code for an intermediate model; without the multiple variances
# win.data <- list(Y      = CO$G,     #Response variable
#                  Xcov  = Xcov,     #Covariates
#                  K     = K,        #Number of covariates
#                  N     = nrow(CO), #Sample size                                 
#                  re    = re,       #Random effect 
#                  Nre   = Nre,      #Number of random effects
#                  X     = XZ$X,     #X
#                  Z     = XZ$Z,     #Z
#                  nU    = ncol(XZ$Z)  #Number of random effects for the smoother
#                  #Habitat  = as.numeric(CO2$fHabitat)
# )
# win.data
# 
# 
# #####################################
# #Model
# #Here we are using univariate Normal priors and not multivariate Normal
# #priors. I think it gives better mixing
# sink("GAMM.txt")
# cat("
#     model{
#     #Priors regression parameters
#     for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) }  
#     
#     #Smoother stuff 
#     for (i in 1: nU) {u[i] ~ dnorm(0, tau.u) }
#     b  ~ dnorm(0, 0.0001) 
#     
#     #Priors for variance random intercept smoother
#     num.u    ~ dnorm(0, 0.0016) 
#     denom.u  ~ dnorm(0, 1)
#     sigma.u <- abs(num.u / denom.u) 
#     tau.u   <- 1 / (sigma.u * sigma.u)
#     
#     #Priors for random intercept 
#     for (i in 1:Nre) {a[i] ~ dnorm(0, tau.a)}
#     
#     #Priors for variance random intercept 
#     num.a    ~ dnorm(0, 0.01) 
#     denom.a  ~ dnorm(0, 1)
#     sigma.a <- abs(num.a / denom.a) 
#     tau.a   <- 1 / (sigma.a * sigma.a)
#     
#     #Prior for sigma eps
#     num.eps    ~ dnorm(0, 0.01) 
#     denom.eps  ~ dnorm(0, 1)
#     sigma.eps <- abs(num.eps / denom.eps) 
#     tau.eps   <- 1 / (sigma.eps * sigma.eps)
#     
#     #Likelihood
#     for (i in 1:N) {
#     Y[i]   ~  dnorm(mu[i], tau.eps)
#     mu[i]  <- eta[i]
#     eta[i] <- inprod(beta[],Xcov[i,]) + F1[i] + a[re[i]]         
#     
#     #Smoother(s)
#     F1[i]  <- b * X[i,1]  + inprod(Z[i,], u[])  
#     
#     #3. Residuals 
#     Exp[i] <- mu[i] 
#     Var[i] <- sigma.eps^2
#     E[i]   <- (Y[i]  - Exp[i]) / sqrt(Var[i])
#     } 
#     }
#     ",fill = TRUE)
# sink()
# #####################################
# 
# 
# 
# #Inits function
# inits  <- function () {
#   list(beta      = rnorm(K, 0, 0.1),  #Regression parameters
#        a         = rnorm(Nre, 0, 1),   #Random effect Survey for count part
#        num.a     = rnorm(1, 0, 25),  #Prior stuff for random effect Survey count part
#        denom.a   = rnorm(1, 0, 1),   #Prior stuff for random effect Survey count part
#        u         = rnorm(ncol(XZ$Z), 0, 1),
#        num.u     = rnorm(1, 0, 25), 
#        denom.u   = rnorm(1, 0, 1),
#        b         = rnorm(1, 0, 0.1),
#        num.eps   = rnorm(1, 0, 25) ,
#        denom.eps = rnorm(1, 0, 1)
#   )  }
# 
# 
# #Parameters to estimate
# params <- c("beta", 
#             "E",
#             "sigma.eps",  "sigma.u", "sigma.a",
#             "u", "b","a", "mu")
# 
# #Start Gibbs sampler
# K1   <- jags(data       = win.data,
#              inits      = inits,
#              parameters = params,
#              model      = "GAMM.txt",
#              n.thin     = 10,
#              n.chains   = 3,
#              n.burnin   = 4000,
#              n.iter     = 5000)
# 
# #This seems to work equally good
# K2  <- update(K1, n.iter = 10000, n.thin = 10)  
# out <- K2$BUGSoutput
# print(out, digits = 3)  
# 
# 
# #5. Assess mixing
# K <- ncol(Xcov)
# MyBUGSChains(out, c(uNames("beta", K), "sigma.a", "sigma.eps"))
# MyBUGSACF(out, c(uNames("beta", K), "sigma.a", "sigma.eps"))
# #Perhaps we should take more iterations?
# 
# 
# 
# #Numerical output
# OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma.a", "sigma.eps"))
# rownames(OUT1)[1:K] <- colnames(Xcov)
# print(OUT1, digits =5)
# 
# 
# #Model validation
# E  <- out$mean$E
# mu <- out$mean$mu
# 
# par(mfrow = c(2,2), mar = c(5,5,2,2))    
# plot(x=mu, y=E, xlab = "Fitted values", ylab ="Residuals")
# abline(h=0, lty=2)
# 
# plot(x=CO$LCC, y = E, xlab = "LCC", ylab = "Residuals")
# abline(h=0, lty=2)
# 
# boxplot(E ~ CO$fHabitat, data = CO)
# boxplot(E ~ CO$Country, data = CO)
# ################################################







################################################
# We still have heterogeneity
# Fit a GAMM with multiple variances
#So..the code below is what we presented in the book

#Covariate matrix
Xcov <- model.matrix(~ 1 + fHabitat, data = CO)
K <- ncol(Xcov)

#Random effect Site
re  <- as.numeric(as.factor(CO$Site))
Nre <- length(unique(re))

#Center covariates
CO$LCC.std <- Mystd(CO$LCC)


#Use O'Sullivan splines
#Get X and Z matrices for LCC.std smoother
#Smoother: X * b + Z * u
numIntKnots <- 14
intKnots <- quantile(unique(CO$LCC.std),
                     seq(0,1,length=(numIntKnots+2))[-c(1,(numIntKnots+2))])

XZ <- OSullivan(CO$LCC.std,
                numIntKnots = 14,
                AddIntercept = FALSE,
                intKnots = intKnots)

#Get all the data fit JAGS
win.data <- list(Y      = CO$G,     #Response variable
                 Xcov  = Xcov,     #Covariates
                 K     = K,        #Number of covariates
                 N     = nrow(CO), #Sample size
                 re    = re,       #Random effect
                 Nre   = Nre,      #Number of random effects
                 X     = XZ$X,     #X
                 Z     = XZ$Z,     #Z
                 nU    = ncol(XZ$Z),  #Number of random effects for the smoother
                 Habitat  = as.numeric(as.factor(CO$fHabitat))
)
win.data


#####################################
#Model
sink("GAMM.txt")
cat("
    model{
    #Priors regression parameters
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001) }

    #Smoother stuff
    for (i in 1: nU) {u[i] ~ dnorm(0, tau.u) }
    b  ~ dnorm(0, 0.0001)

    #Priors for variance random intercept smoother
    num.u    ~ dnorm(0, 0.0016)
    denom.u  ~ dnorm(0, 1)
    sigma.u <- abs(num.u / denom.u)
    tau.u   <- 1 / (sigma.u * sigma.u)

    #Priors for random intercept
    for (i in 1:Nre) {a[i] ~ dnorm(0, tau.a)}

    #Priors for variance random intercept
    num.a    ~ dnorm(0, 0.01)
    denom.a  ~ dnorm(0, 1)
    sigma.a <- abs(num.a / denom.a)
    tau.a   <- 1 / (sigma.a * sigma.a)

    #Prior for sigma eps
    for (i in 1: 14){
    num.eps[i]    ~ dnorm(0, 0.01)
    denom.eps[i]  ~ dnorm(0, 1)
    sigma.eps[i] <- abs(num.eps[i] / denom.eps[i])
    tau.eps[i]   <- 1 / (sigma.eps[i] * sigma.eps[i])
    }


    #Likelihood
    for (i in 1:N) {
    Y[i]   ~  dnorm(mu[i], tau.eps[Habitat[i]])
    mu[i]  <- eta[i]
    eta[i] <- inprod(beta[],Xcov[i,]) + F1[i] + a[re[i]]

    #Smoother(s)
    F1[i]  <- b * X[i,1]  + inprod(Z[i,], u[])

    #3. Residuals
    Exp[i] <- mu[i]
    Var[i] <- sigma.eps[Habitat[i]]^2
    E[i]   <- (Y[i]  - Exp[i]) / sqrt(Var[i])
    }
    }
    ",fill = TRUE)
sink()
#####################################



#Inits function
inits  <- function () {
  list(beta      = rnorm(K, 0, 0.1),  #Regression parameters
       a         = rnorm(Nre, 0, 1),   #Random effect Survey for count part
       num.a     = rnorm(1, 0, 25),  #Prior stuff for random effect Survey count part
       denom.a   = rnorm(1, 0, 1),   #Prior stuff for random effect Survey count part
       u         = rnorm(ncol(XZ$Z), 0, 1),
       num.u     = rnorm(1, 0, 25),
       denom.u   = rnorm(1, 0, 1),
       b         = rnorm(1, 0, 0.1),
       num.eps   = rnorm(14, 0, 25) ,
       denom.eps = rnorm(14, 0, 1)
  )  }


#Parameters to estimate
params <- c("beta",
            "E",
            "sigma.eps",  "sigma.u", "sigma.a",
            "u", "b","a", "mu")

#Start Gibbs sampler
K1   <- jags(data       = win.data,
             inits      = inits,
             parameters = params,
             model      = "GAMM.txt",
             n.thin     = 10,
             n.chains   = 5,
             n.burnin   = 3000,
             n.iter     = 5000)


K2  <- update(K1, n.iter = 8000, n.thin = 10)
out <- K2$BUGSoutput
print(out, digits = 3)


#5. Assess mixing
K <- ncol(Xcov)
MyBUGSChains(out, c(uNames("beta", K),
                    "sigma.a",
                    uNames("sigma.eps", 14)))

MyBUGSACF(out, c(uNames("beta", K),
                 "sigma.a",
                 uNames("sigma.eps", 14)))

#Perhaps we should take more iterations?



#Numerical output
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma.a", uNames("sigma.eps", 14)))
rownames(OUT1)[1:K] <- colnames(Xcov)
print(OUT1, digits =14)


#Model validation
E  <- out$mean$E
mu <- out$mean$mu

#This is not presented in the book....
par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x=mu, y=E, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=CO$LCC, y = E, xlab = "LCC", ylab = "Residuals")
abline(h=0, lty=2)

boxplot(E ~ CO$fHabitat, data = CO)
boxplot(E ~ CO$Country, data = CO)
################################################









################################################
#Sketch smoother: Figure 3.11

#Extract coefficients
u    <- out$sims.list$u
b    <- out$sims.list$b
beta <- out$sims.list$beta

#Create artificial data
range(CO$LCC.std)
ND.LCC <- seq( -1.5 , 2.8, length = 100) #-1.55,  3.53, -1.5,1.5

#Convert this covariate into a smoother basis
XZ.a <- OSullivan(ND.LCC, 
                  numIntKnots = 14, 
                  AddIntercept = FALSE,
                  intKnots = intKnots)


#Calculate the smoothers
I2 <- rep(1, 100)
f1 <-  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)

#Calculate the posterior mean and the 95% credible intervals
f1.info <- MySmoother(f1)


#Plot the smoothers

OriScale <- ND.LCC * sd(CO$LCC) + mean(CO$LCC)



par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = OriScale, 
     y = f1.info[,4], 
     type = "l", 
     xlab = "LCC smoother",
     ylab = "Smoother",
     ylim = c(-5,12),
     cex.lab = 1.5)
lines(OriScale, f1.info[,1], lty=2)
lines(OriScale, f1.info[,3], lty=2)
abline(h=0, lty = 2, col = 2)
################################################################






f1 <-  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)


#Fitted values per habitat
#These are given by:
# = Intercept + correction for habitat + LCC smoother
# = beta1 + correction for habitat + ZX * b + ZX * u

# I.100 <- rep(1, 100)
# #Habitat type 1
# f.H1 <-  I.100 %*% t(beta[,1])  +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 2
# f.H2 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,2]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 3
# f.H3 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,3]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 4
# f.H4 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,4]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 5
# f.H5 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,5]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 6
# f.H6 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,6]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 7
# f.H7 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,7]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 8
# f.H8 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,8]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 9
# f.H9 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,9]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 10
# f.H10 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,10]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 11
# f.H11 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,11]) +
#   XZ.a$X %*% t(b) + XZ.a$Z %*% t(u)
# #Habitat type 12
# f.H12 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,12]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 13
# f.H13 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,13]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
# #Habitat type 14
# f.H14 <-  I.100 %*% t(beta[,1])  + I.100 %*% t(beta[,14]) +
#   XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)

I.100 <- rep(1, 100)
#Habitat type 1
f.H1 <- XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 2
f.H2 <- I.100 %*% t(beta[,2]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 3
f.H3 <-   I.100 %*% t(beta[,3]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 4
f.H4 <-   I.100 %*% t(beta[,4]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 5
f.H5 <-  I.100 %*% t(beta[,5]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 6
f.H6 <-   I.100 %*% t(beta[,6]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 7
f.H7 <-   I.100 %*% t(beta[,7]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 8
f.H8 <-   I.100 %*% t(beta[,8]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 9
f.H9 <-   I.100 %*% t(beta[,9]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 10
f.H10 <-   I.100 %*% t(beta[,10]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 11
f.H11 <-   I.100 %*% t(beta[,11]) +
  XZ.a$X %*% t(b) + XZ.a$Z %*% t(u)
#Habitat type 12
f.H12 <-   I.100 %*% t(beta[,12]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 13
f.H13 <-   I.100 %*% t(beta[,13]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)
#Habitat type 14
f.H14 <-   I.100 %*% t(beta[,14]) +
  XZ.a$X %*% t(b[,1]) + XZ.a$Z %*% t(u)

#Calculate the posterior means and 95% CI
f.H1.info <- MySmoother(f.H1)
f.H2.info <- MySmoother(f.H2)
f.H3.info <- MySmoother(f.H3)
f.H4.info <- MySmoother(f.H4)
f.H5.info <- MySmoother(f.H5)
f.H6.info <- MySmoother(f.H6)
f.H7.info <- MySmoother(f.H7)
f.H8.info <- MySmoother(f.H8)
f.H9.info <- MySmoother(f.H9)
f.H10.info <- MySmoother(f.H10)
f.H11.info <- MySmoother(f.H11)
f.H12.info <- MySmoother(f.H12)
f.H13.info <- MySmoother(f.H13)
f.H14.info <- MySmoother(f.H14)


#Plot the smoothers
OriScale <- ND.LCC * sd(CO$LCC) + mean(CO$LCC)

par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = OriScale, 
     y = f.H14.info[,4], 
     type = "l", 
     xlab = "LCC smoother",
     ylab = "Smoother",
     ylim = c(-5,12),
     cex.lab = 1.5)
#lines(OriScale, f.H1.info[,1], lty=2)
#lines(OriScale, f.H1.info[,3], lty=2)
abline(h=0, lty = 2, col = 2)
lines(OriScale, f.H1.info[,4], lty=1)
lines(OriScale, f.H2.info[,4], lty=1)
lines(OriScale, f.H3.info[,4], lty=1)
lines(OriScale, f.H4.info[,4], lty=1)
lines(OriScale, f.H5.info[,4], lty=1)
lines(OriScale, f.H6.info[,4], lty=1)
lines(OriScale, f.H7.info[,4], lty=1)
lines(OriScale, f.H8.info[,4], lty=1)
lines(OriScale, f.H9.info[,4], lty=1)
lines(OriScale, f.H10.info[,4], lty=1)
lines(OriScale, f.H11.info[,4], lty=1)
lines(OriScale, f.H12.info[,4], lty=1)
lines(OriScale, f.H13.info[,4], lty=1)
lines(OriScale, f.H14.info[,4], lty=1)

#############################################################################

LCC.art<-OriScale
f11<-f.H4[,721]

sum(f.H4[1,]<0)

LengthSeries <- length(LCC.art)	
I3 <- 1:LengthSeries
MyDif <- c(diff(sign(f11)),0)
ThisLCC <- I3[MyDif == 2]
LCCThreshold <- (LCC.art[ThisLCC]+ LCC.art[ThisLCC + 1])/2
max(LCCThreshold)



plot(f.H4[,1],ylim=c(0,30))
for(i in 1:3000){
  lines(f.H4[,i])
}


#############################################################################
#Figure 3.13
#What is the x-value for which a smoother = 0?

for (knock in 1:14){
  G0 <- function(f11, LCC.art ){
    LengthSeries <- length(LCC.art)	
    I3 <- 1:LengthSeries
    MyDif <- c(diff(sign(f11)),0)
    ThisLCC <- I3[MyDif == 2]
    LCCThreshold <- (LCC.art[ThisLCC]+ LCC.art[ThisLCC + 1])/2
    max(LCCThreshold)
  }
  
  LCC.G0 <- vector(length = ncol(f.H1)) 
  for (i in 1:ncol(f1)){
    LCC.G0[i] <- G0(get(paste('f.H',knock,sep=''))[,i], OriScale)     #### switch F.H1 for the other habitats!!!!!!!!!!! 
  }
  f.H1
  LCC.G0[LCC.G0 == -Inf] <- NA  
  
  Range <- quantile(LCC.G0, probs=c(0.05, 0.5, 0.95), na.rm = TRUE)
  
  abline(v=Range[1], lty = 2)
  abline(v=Range[2], lty = 2)
  abline(v=Range[3], lty = 2)
  
  
  
  assign(paste('Hab',knock,sep=''),c(Range[1],
                                     mean(LCC.G0, na.rm = TRUE),
                                     Range[3]))   #use these to put ranges in below
  
}

# Hab1 <- c(8.677993, 13.002616, 19.185747) #found with the Range (quantile line 662
# Hab2 <- c(8.783070, 9.378510, 9.728768 )
# Hab3 <- c(8.677993, 11.974834, 17.241812 )
# Hab4 <- c(2.355657,  6.901762, 11.506364)
# Hab5 <- c(7.653434, 12.586612, 16.804141 )

par(mar = c(10,5,2,2), cex.lab = 1.5)
Myx <- seq(1:14)
Myy <- c(Hab1[2],Hab2[2],Hab3[2],Hab4[2],Hab5[2],Hab6[2],Hab7[2],Hab8[2],Hab9[2],Hab10[2],Hab11[2],Hab12[2],Hab13[2],Hab14[2])
MyLow <- c(Hab1[1],Hab2[1],Hab3[1],Hab4[1],Hab5[1],Hab6[1],Hab7[1],Hab8[1],Hab9[1],Hab10[1],Hab11[1],Hab12[1],Hab13[1],Hab14[1])
MyHig <- c(Hab1[3],Hab2[3],Hab3[3],Hab4[3],Hab5[3],Hab6[3],Hab7[3],Hab8[3],Hab9[3],Hab10[3],Hab11[3],Hab12[3],Hab13[3],Hab14[3])

dat<-cbind( Myy, MyHig, MyLow,levels(CO$fHabitat))
dat<-data.frame(dat);names(dat)[4]<-'reef'
dat<-dat[order(as.numeric(dat$Myy)),]
dat<-cbind(Myx,dat)
dat[, c(1:4)] <- sapply(dat[, c(1:4)], as.numeric)



setwd("E:/ACCRETION OF REEFS/figures 2020")
#tiff('Percent LCC Threshold.png',width=4000,height=2000, res = 300)

#Figure 3.2

par(cex.lab = 1.5, mar = c(7,6,2,2))
boxplot(G ~ fSite, data = CO, xlab = "Site")

plot(0, 0,
     type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "LCC (%) threshold value",
     xlim = c(0.5,14.5),
     ylim = c(0,70)
     ,cex.lab=1.2)

points(dat$Myx, dat$Myy, pch = 16, cex = 1.5)
axis(2)
arrows(dat$Myx, dat$Myy, dat$Myx, dat$MyLow, angle = 90, length = 0.1 )
arrows(dat$Myx, dat$Myy, dat$Myx,dat$MyHig, angle = 90, length = 0.1 )

axis(1, at = c(1:14),
     labels = dat$reef,las=2)
box()

####################################################################
dev.off()







































############## EROSION FIGURE (FIGURE 3)

setwd("E:/ACCRETION OF REEFS/Accretion model 2018/Compare carbonate and SST/G an LCC")
CO<- read_csv('islands data.csv')
CO<-CO[,-1]
str(CO)
names(CO)[3]<-'Net_production'
names(CO)[2]<-'reef'
names(CO)[1]<-'Country'


#CO[,10]<-CO$Accretion_rate-0.06;names(CO)[10]<-'Net_production' #remove sedimentation from accretion
CO[,11]<-CO$BFj+CO$BUj+0.009664352;names(CO)[11]<-'Gross_erosion'

CO$LCC<-CO$LCC/10 #convert from cm/10m to percentage

CO$reef<-paste(CO$Country,CO$reef)
names(CO)[9]<-'Site'
CO$Site<-1:852



###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(R2jags)
library(nlme)
library(mgcv)



##################################################################
#Housekeeping
CO$fCountry <- factor(CO$Country)
CO$fSite    <- factor(CO$Site)
CO$fHabitat <- factor(CO$reef) #switch to outer inner whatnot
CO$G        <- CO$Net_production
##################################################################



##################################################################
#Data exploration
MyVar <- c("G", "LCC")
Mydotplot(CO[,MyVar])

table(CO$fCountry)
table(CO$fSite)
table(CO$fHabitat)
##################################################################


setwd("E:/ACCRETION OF REEFS/figures 2020")
tiff('Carbonate and erosion.png',width=2000,height=1500, res = 300)

ords<-c(1,2,5,6,3,4,10,11,12,13,14,7,8,9)
CO$names <- factor(CO$fHabitat , levels=levels(CO$fHabitat)[ords])

par(cex.lab = 1.5, mar = c(7,5,1,1))
boxplot(G ~ names, data = CO, las=2,xlab='',ylab=expression(paste('Carbonate production (kg','m'^2,'yr'^-1,')    ',sep=' ')),cex.lab=1.2,yaxt='n',col=c(4,5,4,5,3,4,3,4,5,3,4,3,4,5))

boxplot(-100*Gross_erosion ~ names, data = CO, las=2,xlab='',ylab=expression(paste('Carbonate production (kg','m'^2,'yr'^-1,')    ',sep=' ')),cex.lab=1.2,ylim=c(0,3),add=T,col='red', medcol='black', whiskcol='red', staplecol='red', boxcol='black', outcol='red',pch=16,yaxt='n')
axis(side = 2, at = c(-10,0,10,20,30,40), labels = c(-0.1,0,10,20,30,40), tck = -0.01)
box()

dev.off()
