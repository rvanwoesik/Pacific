#van Woesik and Cacciapaglia 2021 PLOS ONE
#Thermal stress jeopardizes carbonate production of coral reefs across the western and central Pacific Ocean 



# SST baseline 1971-1990

#################################################

setwd("C:/RobsR/1 Pacific wide analysis")

#########################################################

source("HighstatLibV10.R")
library(lattice)
library(INLA)
library(ggplot2)
library(ggmap)
library(sp)
library(gstat)
library(ncdf4)
library(raster)
library(spdep)

# set up the data points
allpts<-read.csv('islands data.csv')
library(plyr)

islsdat <- ddply(allpts, c('site',"Island",'P.locat'), summarise,
                 LCC    = mean(LCC),
                 lat = mean(lat),
                 lon = mean(lon),
                 GP = mean (GP),
                 NP = mean (NP)
)
head(islsdat)

############################################################
#Orientation

px<-(islsdat[islsdat$Island=='Palau',]$lon)
py<-(islsdat[islsdat$Island=='Palau',]$lat)
pll<-cbind(px,py)
palau.compass<-c(255,255,105,120,225,80,90,260,300,270,135,240,275,275,315,300,290,300,300,285,290,120,270,270)

yx<-(islsdat[islsdat$Island=='Yap',]$lon)
yy<-(islsdat[islsdat$Island=='Yap',]$lat)
yll<-cbind(yx,yy)
yap.compass<-c(130,310,160,290,315,45,45,295,310,45,135,345,105,165,150,45,315,190,300,285,310,330,330,90)

px<-(islsdat[islsdat$Island=='Pohnpei',]$lon)
py<-(islsdat[islsdat$Island=='Pohnpei',]$lat)
pll<-cbind(px,py)
pohnpei.compass<-c(65,330,210,150,315,135,40,200,345,10,45,135,45,330,350,60,270,240,255,335,0,200,30,240)

kx<-(islsdat[islsdat$Island=='Kosrae',]$lon)
ky<-(islsdat[islsdat$Island=='Kosrae',]$lat)
kll<-cbind(kx,ky)
kosrae.compass<-c(160,230,190,0,25,270,315,105,120,275,110,140,340,45,195,330,300,340,70,300,285,270,210,225)

mx<-(islsdat[islsdat$Island=='Majuro',]$lon)
my<-(islsdat[islsdat$Island=='Majuro',]$lat)
mll<-cbind(mx,my)
majuro.compass<-c(340,20,290,290,225,45,345,200,200,310,40,55,105,15,180,150,30,200,15,15,190,195,220,185)

kx<-(islsdat[islsdat$Island=='Kiritimati',]$lon)
ky<-(islsdat[islsdat$Island=='Kiritimati',]$lat)
kll<-cbind(kx,ky)
kiritimati.compass<-c(225,315,300,150,15,30,180,160,0,210,255,45,315,285,30,285,240,135,285,300,285,30)

compassdir<-c(palau.compass,yap.compass,pohnpei.compass,kosrae.compass,kiritimati.compass,majuro.compass)
allpts$orientation<-rep(compassdir,each=6)


# Derive mean across transects for each site

islsdat <- ddply(allpts, c('site',"Island",'P.locat'), summarise,
                 LCC    = mean(LCC),
                 lat = mean(lat),
                 lon = mean(lon),
                 GP = mean (GP),
                 NP = mean (NP),
                 orientation=mean(orientation)            )

head(islsdat)
islsdat


###################################################################
# Figure 3 Boxplot

par(mar=c(8,7,4.1,2.1))

attach(islsdat)
new_order <- reorder(Island,NP, FUN = mean)
detach(islsdat)

MyYlab <- expression(paste("Net carbonate production ", "(kg CaCO"[3], " m"^"-2", "year" ^"-1",")"))
boxplot(NP~P.locat*new_order,data=islsdat, cex.lab=1.4, cex.axis=1.2, col=c("palegreen2", "blue","skyblue1"), xlab="", ylab=MyYlab, las=2) 


###################################################################
# Some simple stats 

reg=lm(islsdat$NP~islsdat$LCC)
summary(reg)
plot(islsdat$LCC,islsdat$NP)

islsdat$LCC= (islsdat$LCC)/10

library(dplyr)
islsdat %>%
  group_by(Island) %>% #By Island
  dplyr::summarize(Mean = mean(NP, na.rm=TRUE))

islsdat %>%
  group_by(P.locat) %>% #By habitat
  dplyr::summarize(Mean = mean(NP, na.rm=TRUE))

mean(islsdat$NP)
sd(islsdat$NP)


######################################################################
#convert to easting and northing

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

cord.dec = SpatialPoints(cbind(islsdat$lon, islsdat$lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:4326")) #Micronesia
cord.UTM
plot(cord.UTM)

########################################################################

#set up predictor variables

preds<-stack("preds2.nc") 
names(preds)<-c('AnomR','AnomR2','AnomR3','ENSO','PDO','SSTgausDev','storm','PARrange','EpPp','EpPn','EnPp','EnPn')
preds$storm[is.na(preds$storm)]<-0
head(preds)

Freq=stack("mean TSA frequency 2000-2020.nc")
names(Freq)
names(Freq)<-c('Freq')
head(Freq)
Freq<-raster::extract(Freq,coordinates(cbind(islsdat$lon,islsdat$lat)))

# extract 
EXT<-raster::extract(preds,coordinates(cbind(islsdat$lon,islsdat$lat)))
head(EXT)

islsdat$AnomR2<-EXT[,'AnomR2']
islsdat$PDO<-EXT[,'PDO']
islsdat$ENSO<-EXT[,'ENSO']
islsdat$storm<-EXT[,'storm']
islsdat$PARrange<-EXT[,'PARrange']
islsdat$Freq<-Freq
head(islsdat)

#################################################################

#standardize covariates in islsdat

standardize_function<-function(x){
  x.standardized=(x-mean(na.omit(x)))/sd(na.omit(x))
  return(x.standardized)
}

islsdat$AnomR2<-standardize_function(islsdat$AnomR2)
islsdat$PDO<-standardize_function(islsdat$PDO)
islsdat$ENSO<-standardize_function(islsdat$ENSO)
islsdat$storm<-standardize_function(islsdat$storm)
islsdat$PARrange<-standardize_function(islsdat$PARrange)
islsdat$Freq<-standardize_function(islsdat$Freq)

islsdat$habitat=as.factor(islsdat$P.locat)
head(islsdat)

##############################################################

#Collinearity
#Variance inflation factors
MyVar <- c("AnomR2", "PDO", "ENSO", "storm", "Freq", "habitat")
corvif(islsdat[,MyVar])
#Some collinearity

MyVar <- c("AnomR2", "PDO", "storm", "Freq", "habitat")
corvif(islsdat[,MyVar])
#Better

#Relationships
MyMultipanel.ggp2(Z = islsdat, varx = MyVar,vary = "NP", ylab = "Response variable", addSmoother = TRUE,
                  addRegressionLine = FALSE,addHorizontalLine = FALSE)

MyMultipanel.ggp2(Z = islsdat, varx = MyVar, vary = "NP", ylab = "Pearson residuals", addSmoother = TRUE,
                  addRegressionLine = TRUE,addHorizontalLine = TRUE)

library(corrgram)
corrgram(islsdat, order=NULL, lower.panel=panel.shade, upper.panel=NULL)
cor.test(islsdat$AnomR2, islsdat$storm, method=c("pearson")) # -0.79, remove storms

cor.test(islsdat$PDO, islsdat$Freq, method=c("pearson")) #  -0.869 remove PDO                     
#cor.test(islsdat$ENSO, islsdat$storm, method=c("pearson"))  
cor.test(islsdat$ENSO, islsdat$AnomR2, method=c("pearson")) #0.897 
#cor.test(islsdat$ENSO, islsdat$Freq, method=c("pearson"))
#cor.test(islsdat$AnomR2, islsdat$Freq, method=c("pearson"))
cor.test(islsdat$PDO, islsdat$Freq, method=c("pearson")) #-0.869
#Remove storms and PDO

##########################################################
dev.off()
#Gam Figure in supps

library(mgcv)
b <- gam(NP~s(LCC),data=islsdat)
fv <- predict(b,type="terms") ## get term estimates
prsd1 <- residuals(b,type="working") + fv[,1]
plot(b,select=1, xlab = expression(paste("Live coral cover (%)")), cex.lab = 1.5, cex.axis=1.5, 
     ylab = list("s(Net carbonate production)"), shade= TRUE)
ind <- sample(1:length(prsd1),100)
points(islsdat$LCC[ind],prsd1[ind],pch=19,col="black") 

#Using INLA
library(INLA)
# GAM formula
form1 <- makeGAM(c('islsdat$LCC'), response = 'islsdat$NP', invariant = '0 + Intercept')
form2 <- makeGAM(c('islsdat$LCC'), response = 'islsdat$NP', invariant = '0 + Intercept', linear = c('islsdat$LCC'))
observed <- islsdat[1:13, 'NP']
#Epil <- rbind(islsdat, islsdat[1:13, ])
#Epil[1:13, 'NP'] <- NA
formula1 = NP ~ LCC + f(Island, model="iid")
formula2 <- makeGAM('LCC', response='NP', invariant = '', returnstring = FALSE)
formula3 <- makeGAM('LCC', response='NP', invariant = '', linear = c('LCC'), returnstring = FALSE)
result = inla(formula3, family="Gaussian", data = islsdat, 
              control.predictor = list(compute = TRUE, link = 1))
ggplot_inla_residuals(result, observed, binwidth = 0.2)
autoplot(result)



#######################################################################3

#INLA
##############################################
# INLA ANALYSIS
#Fixed and random effects
f1 <- NP ~ AnomR2*Freq + habitat+ f(site, model="iid") + f(Island, model="iid")

I1 <- inla(f1,
             family = "Gaussian", 
             data = islsdat,
             control.compute = list(dic = TRUE))
summary(I1)

#################################################
#Check for overdispersion
ExpY <- I1$summary.fitted.values[,"mean"]
E1   <- (islsdat$NP - ExpY) / sqrt(ExpY)
N <- nrow(islsdat)
p <- length(I2$names.fixed)
Dispersion <- sum(E1^2) / (N - p)
Dispersion

#2.31 is good

####################################################
#Spatial model

Loc <- cbind(islsdat$lon, islsdat$lat)
Loc
plot(Loc)
#dev.off()

#what are the distances between the points?
D <- dist(Loc)
hist(D)

##############################################################

#Setting up mesh

# Mesh
MeshA <- inla.mesh.2d(jitter(Loc), max.edge = c(20, 40))
plot(MeshA)

MeshB <- inla.mesh.2d(Loc, max.edge = c(20, 40))
plot(MeshB)

MeshC <- inla.mesh.2d(Loc, max.edge = c(10, 20))
plot(MeshC)
MeshC$n
#856 is good number. Zurr et al 2017 recommends ~700

#MeshC <- inla.mesh.2d(Loc, max.edge = c(5, 10))
#plot(MeshC)
#MeshC$n

points(islsdat$lon, islsdat$lat, col = "red", pch = 16,  cex = 1)

#############################################################
          
#Create SPDE

clm.spde <- inla.spde2.pcmatern(mesh = MeshC, alpha = 2, #alpha is the default for 2d, not for time series
                                prior.range = c(50, 0.9), # P(range < 50) = 0.9
                                prior.sigma = c(1, 0.01) # P(sigma > 10) = 0.01       )
                                )
 
# Model with only spatial correlation
# Projector matrix

A2 <- inla.spde.make.A(MeshC, loc = Loc)
dim(A2)  #142 observations in a 856 grid


# Define the Matern correlation on the mesh
spde   <- inla.spde2.matern(MeshC, alpha = 2)

#define spatial random field
w.index <- inla.spde.make.index(
                   name    = 'w', 
                   n.spde  = spde$n.spde,
                   n.group = 1,
                   n.repl  = 1)

##########################################################

# Create a data frame with an intercept and covariates.

N <- nrow(islsdat)                
X <- data.frame(Intercept = rep(1, N), 
                AnomR2  = islsdat$AnomR2,
                storm   = islsdat$storm,
                PDO= islsdat$PDO,
                PARrange = islsdat$PARrange,
                ENSO= islsdat$ENSO,
                Freq=islsdat$Freq,
                site = islsdat$site,
                Island= islsdat$Island,
                habitat=islsdat$habitat,
                orientation=islsdat$orientation   )

#Generate Stack

Stk2 <- inla.stack(
             tag  = "Fit",
             data = list(y = islsdat$NP),  
	         A    = list(A2, 1),                      
	         effects = list(                 
	              w = w.index,            #Spatial field  
	              X = as.data.frame(X)))  #Covariates


#############################################################

#Full model with fixed effects, random effects, and spatial random effects (SPDE)

f2 <- y ~ AnomR2*Freq+ habitat+ f(site, model="iid") + f(Island, model="iid") + f(w, model=spde)
#f2 <- y ~ AnomR2+Freq+ENSO+PDO+storm+ habitat+ f(site, model="iid") + f(Island, model="iid") + f(w, model=spde)


I2 <- inla(f2,family = "Gaussian",
             data = inla.stack.data(Stk2),
             control.compute = list(dic = TRUE, waic=TRUE),
             control.predictor = list(A = inla.stack.A(Stk2)))
summary(I2)

#Fixed effects:
 #             mean    sd 0.025quant 0.5quant 0.975quant   mode  kld
#(Intercept)   2.562 0.704      1.070    2.581      3.976  2.617 0.01
#AnomR2       -1.837 0.981     -3.942   -1.806      0.019 -1.786 0.00
#Freq         -0.097 0.647     -1.424   -0.083      1.146 -0.059 0.00
#habitatouter  2.437 0.992      0.488    2.437      4.384  2.438 0.00
#habitatpatch  2.741 1.152      0.509    2.729      5.036  2.704 0.00
#AnomR2:Freq  -0.713 0.644     -1.942   -0.728      0.610 -0.754 0.00

#Full analysis
#Fixed effects:
#               mean    sd 0.025quant 0.5quant 0.975quant   mode kld
#(Intercept)   2.431 0.735      0.933    2.437      3.896  2.452   0
#AnomR2       -2.105 2.014     -6.349   -2.031      1.675 -1.920   0
#Freq         -0.524 0.888     -2.248   -0.533      1.251 -0.549   0
#ENSO         -0.123 1.701     -3.430   -0.136      3.259 -0.162   0
#PDO          -0.621 1.104     -2.861   -0.600      1.496 -0.562   0
#storm         0.493 0.930     -1.294    0.478      2.367  0.450   0
#habitatouter  2.576 1.000      0.609    2.576      4.541  2.577   0
#habitatpatch  2.816 1.196      0.467    2.816      5.165  2.815   0



#####################################################################

#Results
I2$summary.fixed
I2$summary.hyperpar
par(mfrow = c(2, 3))
plot(I2$marginals.fix[[1]], type = "l", xlab = "Intercept", ylab = "Density")
plot(I2$marginals.hy[[1]], type = "l", ylab = "Density", xlab = expression(phi))


#check if there are any spatial relationships with neigboring sites
r.f <- inla.spde2.result(inla=I2, name="w", spde=spde, do.transfer = TRUE)
Kappa= inla.emarginal(function(x) x, r.f$marginals.kappa[[1]])
sigmau=inla.emarginal(function(x) sqrt(x), r.f$marginals.variance.nominal[[1]])
r=inla.emarginal(function(x) x, r.f$marginals.range.nominal[[1]]) 
#Show correlation structure
D1     <- as.matrix(dist(MeshC$loc[,1:2]))
d.vec <- seq(0, max(D1), length = 20)      
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1) 
Cor.M[1] <- 1
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec,y = Cor.M, pch = 16, type = "p", cex.lab = 1.5,xlab = "Distance (km)", 
     ylab = "Correlation")

################################################
#Graphics
#devtools::install_github('timcdlucas/INLAutils')
library(INLAutils)
p <- autoplot(I2)
cowplot::plot_grid(plotlist = p)

#####################################################
# Figure 6, fixed effects

par(mar=c(5,7,4.1,2.1))
par(mfrow=c(1,3))

#SST
plot(I2$marginals.fix[[2]], type = "l", xlab = "SST anomaly", xlim=c(-6,6), cex.axis=1.3,ylab = "Density", cex.lab=2)
abline(v=0, col="black", lty=2)
polygon(I2$marginals.fix[[2]], col=rgb(1,0,0,1, alpha=0.3) )
text(-5,0.52,"a", cex=2)

#Frequency of SST events
plot(I2$marginals.fix[[3]], type = "l", xlab = "SST frequency", xlim=c(-6,6), cex.axis=1.3,ylab = "Density", cex.lab=2)
abline(v=0, col="black", lty=2)
polygon(I2$marginals.fix[[3]], col=rgb(0,1,0,0, alpha=0.3) )
text(-5,0.62,"b", cex=2)

#Frequency*SST
plot(I2$marginals.fix[[6]], type = "l", xlab = "SST anomaly*SST frequency", xlim=c(-6,6), cex.axis=1.3,ylab = "Density", cex.lab=2)
abline(v=0, col="black", lty=2)
polygon(I2$marginals.fix[[6]], col=rgb(1,0,0,1, alpha=0.3) )
text(-5,0.62,"c", cex=2)

#PDO
#plot(I2$marginals.fix[[4]], type = "l", xlab = "PDO", ylab="", xlim=c(-6,6),cex.axis=1.3,cex.lab=2)
#abline(v=0, col="black",lty=2)
#polygon(I2$marginals.fix[[4]], col=rgb(0,1,0,0, alpha=0.3) )

#Cyclones
#plot(I2$marginals.fix[[5]], type = "l", xlab = "Cyclones", ylab = "",xlim=c(-6,6),cex.axis=1.3, cex.lab=2)
#abline(v=0, col="black",lty=2)
#polygon(I2$marginals.fix[[5]], col=rgb(0,1,0,0, alpha=0.3) )

dev.off()

#Habitat Outer
#plot(I2$marginals.fix[[6]], type = "l", xlab = "Outer reefs", xlim=c(-10,10),ylab = "",cex.axis=1.3, cex.lab=2)
#abline(v=0, col="black",lty=2)
#polygon(I2$marginals.fix[[6]], col=rgb(0,0,1,1, alpha=0.3) )

#habitat patch
#plot(I2$marginals.fix[[7]], type = "l", xlab = "Patch reefs", xlim=c(-10,10),ylab = "",cex.axis=1.3, cex.lab=2)
#abline(v=0, col="black",lty=2)
#polygon(I2$marginals.fix[[7]], col=rgb(0,0,1,1, alpha=0.3) )


################################################################


# A few extra plots

#check Model$summary.random$w$mean
round(I2$summary.fixed,3)
round(I2$summary.hyperpar[1,],3)
plot(I2$marginals.fixed[[2]], type = "l", 
     xlab = expression(Anomalous_SST), ylab = "density")

plot(I2$marginals.fixed[[3]], type = "l", 
     xlab = expression(PDO), ylab = "density")

# posterior marginal for e is required
post.se <- inla.tmarginal(function(x) sqrt(1 / exp(x)),
                          I2$internal.marginals.hyperpar[[1]])

# inla.emarginal() is used to compute the posterior expectation of a function on the parameter, 
# inla.qmarginal() computes quantiles from the posterior marginal, 
# inla.hpdmarginal() computes a highest posterior density (HPD) interval and 
# inla.pmarginal() can be used to obtain posterior probabilities.

inla.emarginal(function(x) x, post.se)
inla.qmarginal(c(0.025, 0.5, 0.975), post.se)
inla.hpdmarginal(0.95, post.se)
inla.pmarginal(c(0.5, 0.7), post.se)

#plot Posterior marginals of the precision and the standard deviation
par(mfrow = c(1, 2), mar = c(3, 3, 1, 1), mgp = c(2, 1, 0))
plot(I2$marginals.hyperpar[[1]], type = "l", xlab = "precision", 
     ylab = "density", main = "Precision")
plot(post.se, type = "l", xlab = "st. deviation", 
     ylab = "density", main = "Standard deviation")

#######################################################


#Projecting across a grid
pgrid0 <- inla.mesh.projector(MeshC, xlim = 0:1, ylim = 0:1,dims = c(101, 101))
prd0.m <- inla.mesh.project(pgrid0,  I2$summary.random$w$mean)
prd0.s <- inla.mesh.project(pgrid0,  I2$summary.random$w$sd)


############################################################
# Try different models

# Fixed effects only
f5 <- y ~ -1 + AnomR2 + habitat
I5 <- inla(f5, family = "Gaussian", 
           data = inla.stack.data(Stk2),
           control.compute = list(dic = TRUE, waic=TRUE),
           control.predictor = list(A = inla.stack.A(Stk2)))
summary(I5)
#DIC=821,waic=821

# fixed + ID random effects,
f6 <- y ~ -1 + AnomR2 + habitat+ f(site, model="iid") + f(Island, model="iid") 
I6 <- inla(f6, family = "Gaussian", 
           data = inla.stack.data(Stk2),
           control.compute = list(dic = TRUE,waic=TRUE),
           control.predictor = list(A = inla.stack.A(Stk2)))
summary(I6)
#DIC=821,waic=821

#just the spatial model and net carbonate
f7 <- y ~ -1 + f(w, model=spde)

I7 <- inla(f7, family = "Gaussian", 
           data = inla.stack.data(Stk2),
           control.compute = list(dic = TRUE,waic=TRUE),
           control.predictor = list(A = inla.stack.A(Stk2)))
summary(I7)
#DIC= 820, waic=820


##########################################################

# Nice figure, but not used in paper

library(ggimage)
#if(!require(ggregplot)) 
#devtools::install_github("gfalbery/ggregplot") # Installing Greg's package for plotting functions!

library(ggregplot)
Loc1=data.frame(Loc)

ggField(I2, MeshC) +
  scale_fill_brewer(palette = "YlOrBr") +
  geom_point(data = Loc1, inherit.aes = F, 
             aes(Loc1[,1], Loc1[,2]),  colour = "Black", cex=3) +
  annotate("text", x = 202.5, y = 4, label = "Kiritimati") +
  annotate("text", x = 171, y = 9.2, label = "Majuro") +
  annotate("text", x = 163, y = 3.2, label = "Kosrae") +
  annotate("text", x = 158.3, y = 8.9, label = "Pohnpei") +
  annotate("text", x = 138.2, y = 11.5, label = "Yap") +
  annotate("text", x = 134.3, y = 5.2, label = "Palau") 



dev.off()
###########################################################FIGURE 7a

#Figure 7a
f2 <- y ~ AnomR2+ PDO+ habitat+ f(site, model="iid") + f(Island, model="iid") + f(w, model=spde)

I2 <- inla(f2,family = "Gaussian",
           data = inla.stack.data(Stk2),
           control.compute = list(dic = TRUE, waic=TRUE),
           control.predictor = list(A = inla.stack.A(Stk2)))
summary(I2)
Q = inla.spde.precision(spde, theta = c(0,0))
sample = inla.qsample(n = 2, Q)
proj <- inla.mesh.projector(MeshC, dims = c(300, 300))
sample_proj = inla.mesh.project(proj, field = I2$summary.random$w$mean)

library(fields)
image.plot(proj$x, proj$y, sample_proj, legend.args=list(text="Posteriori mean spatial random effect", cex=1.2, side=4, line=2.5),xaxt='n', yaxt='n',legend.cex=1.2, col=rev(tim.colors()),legend.shrink=1, xlab="Longitude", ylab="Latitude", cex.lab=1.5, cex.axis=1.2)

#image(proj$x, proj$y, sample_proj, xlab="Longitude", ylab="Latitude", cex.lab=1.5, cex.axis=1.2)
points(islsdat$lon, islsdat$lat, col = "black", pch = 16,  cex = 1)
legend=TRUE
library(rworldmap)
library(maptools)
library(raster)
library(rworldxtra)==
library(fields)
wholeworld<-getMap(resolution="high")
#Micronesia<-crop(wholeworld, c(-30,180,-15, 25)) 
plot(wholeworld, add=TRUE)
text(202.5, 4,"Kiritimati") 
text(171, 9.2, "Majuro")
text(163, 3.2,"Kosrae")
text(158.3, 8.9,"Pohnpei")
text(138.2, 11.5,"Yap")
text(134.3, 5.2,"Palau")
text(125,22,"a",cex=2)

axis(1, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
text(c(160, 200), -10, c("140°E", "160°W"))
text(213,-10,"10°S")
text(213,0,"0°")
text(213,10,"10°N")

# Run compass row function at end of this script

compassRose(200,17,cex=.5,cex.dir=1.2,llwd=1.5)

text(170,19,'Pacific Ocean',cex=1.2)
scalebar(d=10,xy=c(195,11.5),label=c(0,'',1000),cex=.5,type='bar',divs=4,below="kilometers",adj=c(0.5,-1.1))





#######################################################FIGURE 7b
#Figure 7b

f2 <- y ~ AnomR2*Freq+ habitat+ f(site, model="iid") + f(Island, model="iid") + f(w, model=spde)

I2 <- inla(f2,family = "Gaussian",
           data = inla.stack.data(Stk2),
           control.compute = list(dic = TRUE, waic=TRUE),
           control.predictor = list(A = inla.stack.A(Stk2)))
summary(I2)

#making a projection
Q = inla.spde.precision(spde, theta = c(0,0))
sample = inla.qsample(n = 2, Q)
proj <- inla.mesh.projector(MeshC, dims = c(300, 300))
sample_proj = inla.mesh.project(proj, field = I2$summary.random$w$mean)


#image(proj$x, proj$y, sample_proj, xlab="Longitude", ylab="Latitude", cex.lab=1.5, cex.axis=1.2)
image.plot(proj$x, proj$y, sample_proj, legend.args=list(text="Posteriori mean spatial random effect", cex=1.2, side=4, line=2.5),col=rev(tim.colors()),legend.shrink=1, xaxt='n', yaxt='n',xlab="Longitude", ylab="Latitude", cex.lab=1.5, cex.axis=1.2)

wholeworld<-getMap(resolution="high")
#Micronesia<-crop(wholeworld, c(-30,180,-15, 25)) 
plot(wholeworld, add=TRUE)
text(202.5, 4,"Kiritimati") 
text(171, 9.2, "Majuro")
text(163, 3.2,"Kosrae")
text(158.3, 8.9,"Pohnpei")
text(138.2, 11.5,"Yap")
text(134.3, 5.2,"Palau")
text(125,22,"a",cex=2)

axis(1, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
text(c(160, 200), -10, c("140°E", "160°W"))
text(213,-10,"10°S")
text(213,0,"0°")
text(213,10,"10°N")

# Run compass row function at end of this script
compassRose(200,17,cex=.5,cex.dir=1.2,llwd=1.5)

text(170,19,'Pacific Ocean',cex=1.2)
scalebar(d=10,xy=c(195,11.5),label=c(0,'',1000),cex=.5,type='bar',divs=4,below="kilometers",adj=c(0.5,-1.1))



###############################################################
source("HighstatLibV11.R")
PlotField(field= I2$summary.random$w$mean, mesh=MeshC,
          xlim=range(MeshC$loc[,1]),
          ylim=range(MeshC$loc[,2]))



##################################################################################
#Standard deviation
sample_proj.s = inla.mesh.project(proj, field = I2$summary.random$w$sd)

image(proj$x, proj$y, sample_proj.s, xlab="Longitude", ylab="Latitude", cex.lab=1.5, cex.axis=1.2)
points(islsdat$lon, islsdat$lat, col = "black", pch = 16,  cex = 1)
library(rworldmap)
library(maptools)
library(raster)
library(rworldxtra)
wholeworld<-getMap(resolution="high")
#Micronesia<-crop(wholeworld, c(-30,180,-15, 25)) 
plot(wholeworld, add=TRUE)
text(202.5, 4,"Kiritimati") 
text(171, 9.2, "Majuro")
text(163, 3.2,"Kosrae")
text(158.3, 8.9,"Pohnpei")
text(138.2, 11.5,"Yap")
text(134.3, 5.2,"Palau")
text(125,22,"a",cex=2)

axis(1, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(120,220,20), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=seq(-10,20, 10), NA, cex.axis=.7, font=1, tck=.02)
text(c(160, 200), -10, c("140°E", "160°W"))
text(213,-10,"10°S")
text(213,0,"0°")
text(213,10,"10°N")

# Run compass row function at end of this script

compassRose(200,17,cex=.5,cex.dir=1.2,llwd=1.5)

text(170,19,'Pacific Ocean',cex=1.2)
scalebar(d=10,xy=c(195,11.5),label=c(0,'',1000),cex=.5,type='bar',divs=4,below="kilometers",adj=c(0.5,-1.1))




###################

compassRose<-function(x,y,rot=0,cex=1,cex.dir=1,llwd=1) {
  oldcex<-par(cex=cex)
  mheight<-strheight("M")
  xylim<-par("usr")
  plotdim<-par("pin")
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180
  crspans<-rep(c(mheight*3,mheight/2),4)
  xpoints<-cos(point.angles)*crspans*xmult+x
  ypoints<-sin(point.angles)*crspans+y
  polygon(xpoints,ypoints,lwd=llwd)
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y
  text(txtxpoints,txtypoints,c("E","N","W","S"),cex=cex.dir)
  par(oldcex)
} 







#####################################################################
#Another way of presenting the spatial random field, Zurr et al 2017
w.pm <- I2$summary.random$w$mean 
#w.pm<-I2$summary.fitted.values$mean
w.proj <- inla.mesh.projector(MeshC) 
w.pm100_100 <- inla.mesh.project(w.proj, w.pm)
grid <- expand.grid(x = w.proj$x, y = w.proj$y)
grid$z <- as.vector(w.pm100_100)          
library(grid)   
head(grid)  
levelplot(z ~ x * y, data = grid, aspect = "iso",scales = list(draw = TRUE),
          xlab = list("Longitude", cex = 1.5),ylab = list("Latitude", cex = 1.5),
          col.regions = heat.colors(100),
          main = list("", cex = 1.5),
          panel=function(...){
            panel.levelplot(...)
            grid.points(x = islsdat$lon, y = islsdat$lat, pch = 1,size = unit(0.5, "char"))}  )


########################################################################

# Model validation
I2a <- I2$summary.fitted.values[1:142,"mean"]
FitIndex <- inla.stack.index(Stk2, tag = "Fit")$data
Fit2a <- I2$summary.fitted.values[FitIndex,"mean"]
E2 <- islsdat$NP - Fit2a
plot(x = Fit2a, y = E2)
# The residuals look good, low values are a little unusual, probably low values on Palau east coast because of the recent cyclones


# Using SLOOCV spatial leave-one out cross-validation
library(INLAutils)
out.field <- inla.spde2.result(I2,'w', spde, do.transf = TRUE)
range.out <- inla.emarginal(function(x) x, out.field$marginals.range.nominal[[1]])
ss <- 20 # sample size to process (number of SLOO runs)
# define the radius of the spatial buffer surrounding the removed point.
# Make sure it isn't bigger than 25% of the study area (Le Rest et al.(2014))
rad <- min(range.out, max(dist(coords)) / 4)
alpha <- 0.05 # RMSE and MAE confidence intervals (1-alpha)
set.seed(199)

#f1<- -1+ AnomR2+storm+habitat
#f10 <- y ~ AnomR2+Freq+ storm + f(site, model="iid") + f(Island, model="iid") + f(spatial.field, model=spde) 
f11<- y ~ AnomR2*Freq + storm+ habitat+ f(site, model="iid") + f(Island, model="iid") + f(spatial.field, model=spde)

# run the function to compare models I2 and I5
cv <- inlasloo(dataframe = islsdat,long = 'lon', lat = 'lat',
               y = 'NP', ss = ss, rad = rad, modform = list(f11),
               mesh = MeshC, family = 'normal',mae = TRUE)


#######################################################################
# Check spatial dependency
library(sp)
library(gstat)
islsdat$Xkm <- islsdat$lon
islsdat$Ykm <- islsdat$lat
MyData <- data.frame(islsdat$NP, islsdat$Xkm, islsdat$Ykm)
coordinates(MyData) <- c("islsdat.Xkm", "islsdat.Ykm")
Vario <- variogram(object = islsdat.NP ~ 1, data = MyData, cressie = TRUE)
plot(Vario)
#Around 4 km at a reef scale.


#######################################################################




