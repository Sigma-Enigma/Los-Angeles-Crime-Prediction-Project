
require(rgdal)
require(sp)
require(maps)
require(spdep)
require(maptools)
require(rgeos)
require(dplyr)
require(spgrass6)
require(ggplot2)
require(gpclib)
require(RColorBrewer)
require(spatstat)
require(MASS)
require(PtProcess)
require(raster)
require(geostatsp)

########### START BIG LIST OF PLOTS ############

# List of plots


# post relevent files here

# plot of study area
spplot(zb2, "BURG_COUNT", main="2016 Annual Burglary Counts in LA County ", col.regions=pal3, at = c(0, .99, 25,50, 75, 100, 125, 150, 175, 200, 225)) 


plot(im.burg.avg.13to15, main = expression( paste( lambda[bar(k)] ,', burglaries \\ mi'^'2' ) ) )

plot.quadratcount( quadratcount(ppp16, 10), main= "Example 10x10 Quadrature Scheme" )

spplot(zb2, "BURG_COUNT", main="2016 Annual Burglary Counts in LA County ", col.regions=pal3, at = c(0, .99, 25,50, 75, 100, 125, 150, 175, 200, 225)) 

plot(ppm16.baseline)

plot(ppm16.reduced2.offset.densavg)

ppm16.baseline.residuals$val[c(4430,4524)] <- c(0,0)
ppm16.baseline.residuals$density[c(4430,4524)] <- c(0,0)
plot.msr(ppm16.baseline.residuals, pch=".")
sqrt(integral(ppm16.baseline.residuals^2))

ppm16.r2.o.davg.residuals$val[c(4430,4524)] <- c(0,0)
ppm16.r2.o.davg.residuals$density[c(4430,4524)] <- c(0,0)
plot.msr(ppm16.r2.o.davg.residuals, pch=".")
sqrt(integral(ppm16.r2.o.davg.residuals^2))


# baseline raw residual plots
plot(residuals(ppm16.baseline, type="pearson")[-c(4430,4524)], main="", pch=".")
# smoothed pearson residual plot DONT USE
plot(Smooth(residuals(ppm16.baseline, type="pearson")[-c(4430,4524)]), main="") # ppm16 baseline smoothed residuals

# reduced model raw residual plot
plot(residuals(ppm16.reduced2.offset.densavg, type="raw")[-c(4430,4524)], main="", pch=".")
# smoothed pearson residual plot DONT USE
plot(Smooth(residuals(ppm16.reduced2.offset.densavg, type="pearson"), diggle=TRUE, leaveoneout=TRUE, sigma= bw.ppl()), main="" ) #ppm16 full model smoothed residuals


diagnose.ppm(ppm16.baseline, type="pearson", pch=".")
#qqplot.ppm(ppm16.baseline)
#qqplot.ppm16.basesline <- .Last.value

diagnose.ppm(ppm16.reduced2.offset.densavg, type="pearson", pch=".")
#qqplot.ppm16.r2.o.davg <- qqplot.ppm(ppm16.reduced2.offset.densavg)

plot(qqplot.final.ppm16.r2.o.davg) 
# fat tail QQ plot
# suggests fewer large residuals than we would theoretically expect via the simulations

plot(inf.baseline, pch=".", main = "") #all equal leverage

plot(inf.reduced, symap =  symbolmap( shape= "circles", pchs=c(".",16,16,16), cols=c("black", "green", "cyan", "violet"), bgs=c("black", "green", "cyan", "violet") ), main="") # 4 different leverage levels


#Kinhom estimate AND bootstrapped confidence intervals by simulating points using ppm and refitting kinhom estimate function
plot(kinhom.env.ppm16.r2.densavg3.offset, xlim = c(0,1.5))
# plot(kinhom.env.ppm16.r2.densavg3.offset, xlim = c(0,1.5))
plot(kinhom.env.ppm16.r2.densavg3.offset, xlim = c(0,.65))


Kres(ppm16.reduced2.offset.densavg)


## need residual sum of squares table


## To see overall fit use density - predicicted estimates plots and find residual measure



# then to get more technical use the 500x500 pixel image conf ints as lower and upper bounds for intensities 

# 95% prediction interval
preds5.predint <- preds5.confint
preds5.predint$confidence$`2.5%`$v[which(preds5.predint$confidence$`2.5%`$v < 0)] <- 0

cf <- colorRampPalette( c("navy", "blue3", "darkorchid4", "violetred3", "red2", "orangered2", "orange",  "goldenrod1", "yellow"))
pal4 <- cf(1000)

# most extreme prediction interval accounting for uncertainty in parameter estimate
# adjust plot color so ribbon is tied to value
plot( as.im.matrix( t( matrix( qpois(0.025, as.vector(as.matrix.im(preds5.predint$confidence$`2.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Lower 95% Prediction Critical Value", col = colourmap( col = pal4, range=c(0,202) ) )
#plot( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Upper 95% Prediction Critical Value", valuesAreColours= TRUE)
plot( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Upper 95% Prediction Critical Value", col = colourmap(col= pal4, range=c(0,202) ) )

# plot the 95% count prediction interval and show that the real data 
#is betwen these values 4285 points within c(4203, 4461)

# why isnt the sum of the intensities equaling the predicted counts???
sum(as.im.matrix( t( matrix( qpois(0.025, as.vector(as.matrix.im(preds5.predint$confidence$`2.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ))^.5
sum( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ) )^.5

sum(!is.na(preds5.ci.128$confidence$`2.5%`$v)) # number of 128x128 pixels with data
area(preds5.ci.128$confidence$`2.5%`)

# area in sq-miles per 128x128 pixel
p.area.128 <- area(preds5.ci.128$confidence$`97.5%`)/ sum(!is.na(preds5.ci.128$confidence$`97.5%`$v))  # equals area in sq-miles per pixel

# area in sq-miles per 500x500 pixel
p.area.500 <- area(preds5.confint$confidence$`97.5%`)/ sum(!is.na(preds5.confint$confidence$`97.5%`$v))  # equals area in sq-miles per pixel

# lower prediction interval of expected counts
sum(preds5.ci.128$confidence$`2.5%`) * area(preds5.ci.128$confidence$`2.5%`)/ sum(!is.na(preds5.ci.128$confidence$`2.5%`$v)) 

# upper prediction interval of expected counts
sum(preds5.ci.128$confidence$`97.5%`) * area(preds5.ci.128$confidence$`97.5%`)/ sum(!is.na(preds5.ci.128$confidence$`97.5%`$v)) 

# lower prediction interval of expected counts
sum(preds5.confint$confidence$`2.5%`) * area(preds5.confint$confidence$`2.5%`)/ sum(!is.na(preds5.confint$confidence$`2.5%`$v)) 

# upper prediction interval of expected counts
sum(preds5.confint$confidence$`97.5%`) * area(preds5.confint$confidence$`97.5%`)/ sum(!is.na(preds5.confint$confidence$`97.5%`$v)) 

# why are these intervals too large??? might be due to rescaling of values to miles from original pixel unit

# total window area * proportion of pixels with data = 345.578 or total area
# confirms area is does not include NA pixels
(o1.15$xrange[2] - o1.15$xrange[1]) * (o1.15$yrange[2] - o1.15$yrange[1]) * (sum(!is.na(preds5.ci.128$confidence$`2.5%`$v))/128^2)
(o1.15$xrange[2] - o1.15$xrange[1]) * (o1.15$yrange[2] - o1.15$yrange[1]) * (sum(!is.na(preds5.confint$confidence$`2.5%`$v))/500^2)


preds5.ci.128$confidence$`2.5%`$v[which(preds5.ci.128$confidence$`2.5%`$v < 0 )] <- 0


# plotting widest prediction intervals (with uncertainty in intensity parameter)
im.preds5.preint.lower <- as.im.matrix( t( matrix( qpois(0.025, as.vector(as.matrix.im(preds5.ci.128$confidence$`2.5%`*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )
#plot( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Upper 95% Prediction Critical Value", valuesAreColours= TRUE)
im.preds5.preint.upper <- as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.ci.128$confidence$`97.5%`*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )

pixel.raster.ppp17 <- pixellate.ppp(ppp17, dimyx=c(128,128) )

# PLOT OF PROBLEM AREAS
plot( (pixel.raster.ppp17 >= im.preds5.preint.lower) & (pixel.raster.ppp17 <= im.preds5.preint.upper) , main="Regions within 95% Prediction Interval")

# EXPANDED MODEL
# narrow prediction interval expanded model
im.final.preds5.predint.lower <- as.im.matrix( t( matrix( qpois(0.025, as.vector(as.matrix.im(preds5.128$estimate*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )
#plot( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Upper 95% Prediction Critical Value", valuesAreColours= TRUE)
im.final.preds5.predint.upper <- as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.128$estimate*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )

# CONSERVATIVE PLOT OF PROBLEM PIXELS , residual hypothesis testing 
plot( (pixel.raster.ppp17 >= im.final.preds5.predint.lower) & (pixel.raster.ppp17 <= im.final.preds5.predint.upper), main= "") # Regions within 95% Prediction Interval for Expanded Model
###### !!!!!!!!!! KEY PLOT ABOVE THIS LINE !!!!!!!!!!! ######
im.under <- (pixel.raster.ppp17 >= im.final.preds5.predint.lower)
im.over  <- (pixel.raster.ppp17 <= im.final.preds5.predint.upper)
# 104 pixels fail to fall within the 95% prediction interval
sum((pixel.raster.ppp17 >= im.final.preds5.predint.lower) & (pixel.raster.ppp17 <= im.final.preds5.predint.upper))


# BASELINE MODEL
# narrow prediction interval baseline model
im.final.preds.base.lower <- as.im.matrix( t( matrix( qpois(0.025, as.vector(as.matrix.im(preds.base.128$estimate*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )
#plot( as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds5.predint$confidence$`97.5%`, na.rm = TRUE) ) ), nrow = 500, ncol = 500, byrow = TRUE) ), W= o1.15, dimyx = c(500,500) ), main = "Upper 95% Prediction Critical Value", valuesAreColours= TRUE)
im.final.preds.base.upper <- as.im.matrix( t( matrix( qpois(0.975, as.vector(as.matrix.im(preds.base.128$estimate*p.area.128, na.rm = TRUE) ) ), nrow = 128, ncol = 128, byrow = TRUE) ), W= o1.15, dimyx = c(128,128) )

# CONSERVATIVE PLOT OF PROBLEM PIXELS , residual hypothesis testing 
plot( (pixel.raster.ppp17 >= im.final.preds.base.lower) & (pixel.raster.ppp17 <= im.final.preds.base.upper), main="") # Regions within 95% Prediction Interval for Baseline Model
###### !!!!!!!!!! KEY PLOT ABOVE THIS LINE !!!!!!!!!!! ######
im.under <- (pixel.raster.ppp17 >= im.final.preds.base.lower) ; plot(im.under)
im.over  <- (pixel.raster.ppp17 <= im.final.preds.base.upper) ; plot(im.over)
# 110 pixels fail to fall within the 95% prediction interval
sum(!((pixel.raster.ppp17 >= im.final.preds.base.lower) & (pixel.raster.ppp17 <= im.final.preds.base.upper)))

im.pix.base.c <- (pixel.raster.ppp17 >= im.final.preds.base.lower) & (pixel.raster.ppp17 <= im.final.preds.base.upper)
im.pix.expa.c <- (pixel.raster.ppp17 >= im.final.preds5.predint.lower) & (pixel.raster.ppp17 <= im.final.preds5.predint.upper)
#### !!!!!!! OTHER KEY GRAPH BELOW !!!!!!!!!!!  ############
plot(im.pix.expa.c - im.pix.base.c, col=c("red", "black", "green"), main = "") # improvement in expanded model fit, green = expanded better, red = baseline better

# EXTRA THINGS TO DO IF YOU HAVE TIME
# consider redoing plots so that:
# 1) scales comparing plots have identical range (colors span largest range of the two, see lines 1613-1620 i.e. colourmap)
# 2) intensities are rescaled to be proportional to the size of the pixels used 
# ppm16.expanded$internal 



# old interval code
# as.matrix.im(preds5.confint$confidence$`97.5%`$v)
# 
# lambda.vector.lower <- as.vector( preds5.confint$confidence$`2.5%`$v )
# #lambda.vector.lower[which( is.na(lambda.vector.lower) == TRUE)] <- 0
# lambda.vector.lower[which( (lambda.vector.lower) < 0)] <- 0
# 
# lambda.vector.upper <- as.vector(preds5.confint$confidence$`97.5%`$v )
# #lambda.vector.upper[which( is.na(lambda.vector.upper) == TRUE)] <- 0
# 
# preds5.lower <- qpois(0.025, lambda=lambda.vector.lower)
# preds5.lower <- as.matrix.default(preds5.lower, nrow=500, ncol=500)
# 
# preds5.upper <- qpois(0.975, lambda=lambda.vector.upper)
# preds5.upper <- as.matrix.default(preds5.upper, nrow=500, ncol=500)
# 
# preds5.predint <- preds5.confint
# names(preds5.predint) <- c("prediction", "se")
# 
# im.preds5.lower <- as.im(preds5.lower, W = o1.15)
# im.preds5.upper <- as.im(preds5.upper, W = o1.15)
# 
# 
# preds5.predint$prediction$`2.5%`$v<- preds5.lower
# preds5.predint$prediction$`97.5%`$v <- preds5.upper
# 
# plot(preds5.predint$prediction$`2.5%` )
# 
# im(pred5.lower, x)

# create lower prediction interval vector
# convert vector back to 500x500 pixle image

# repeat for upper prediction interval vector



# replace values (4430, 4524) with zeros or interpolated values and rerun code


im.income.hetero <- im.a1^2 + im.a2^2 + im.a3^2 + im.a4^2 + im.a5^2 + im.a6^2
# testing new variable consolidating tax bracket measure
ppm16.r.final <-  ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + income.hetero + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, income.hetro=im.income.hetro, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)



cor(im.income.hetro, im.pov.allfams)



##### edits for revisions #####


# main ppm model ppm16.reduced2.offset.densavg
# ppp16 and ppp17 are, respectively, the training and validation response variables
# pppPriorTo16 = jittered burg locations of year 2016

dirich16 <- dirichlet(pppPriorTo16) 
save(dirich16, file = "dirich16.RData")

# need tessellated plot of model estimates (with estimate values, given by inverse of area?)
# need counts of burgs within each tesselation (with counts attached to each tile)


quadcount16.estimate <- quadratcount(pppPriorTo16, tess = dirich16)

# rebuild model with dirichlet quadrature scheme (might need to rebuild covariate data too... SIGH... NVM!)
quad.ppm(ppm16.reduced2.offset.densavg)

# If i wanted to redo my model using voronoi quadrature scheme I would do this
dir.qscheme <- quadscheme(pppPriorTo16, method="dirichlet", exact=TRUE)
voronoi.ppm16.reduced2.offset.densavg <- ppm(Q = dir.qscheme, trend = ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)
quad.ppm(voronoi.ppm16.reduced2.offset.densavg)


# However I will avoid this and merely use the voronoi residual scheme (for grouping points) in place of a pixel residual scheme
# So instead I will...

# using dirichlet tessellation of true model realization (hard data),
# 1) simulate 1000 samples and store the 1000 counts for each tessellation,
# 2) take the mean and SD of these values and store them to the respective tessellations,
# 3) sum the intensity weights (of ppm model) of all pixels in each tessellation  (use centroids to determine if inside) 
# 4) step 3 can be replaced by (point counts in tess / area of tess) which is equal to the estimated intensity of the real data using the voronoi tessellation scheme


simsfinal <- rmh.ppm( ppm16.reduced2.offset.densavg, nsim = 1000, w = o1.15, verbose = TRUE, progress = TRUE )
sims.ppm16.1 <- simsfinal
sims.ppm16.base.1 <- rmh.ppm( ppm16.baseline, nsim = 1000, w = o1.15, verbose = TRUE, progress = TRUE )


# old directories and files
# setwd("D:/Thesis Paper")
# save( sims.ppm16.1, file="sims.ppm16.1.RData")
# save( sims.ppm16.base.1, file="sims.ppm16.base.1.RData")



# preds5 <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = im.data2, ngrid=500, type="trend", se=TRUE)
# ppm16.reduced2.offset.densavg <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)

#cv.ppm16.baseline <- update.ppm( ppm16.baseline, covariates = im.data2)
cv.ppm16.baseline <- ppm( ppp16 ~ offset(log(burg.dens.prior)) , covariates = im.data2, subset = o1.15)
#cv.ppm16.reduced2.offset.densavg <- update.ppm( ppm16.reduced2.offset.densavg, covariates = im.data2)
cv.ppm16.reduced2.offset.densavg <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = im.data2, subset = o1.15)

sims.cv.ppm16.1 <- rmh.ppm( ppm16.reduced2.offset.densavg, nsim = 1000, w = o1.15, verbose = TRUE, progress = TRUE )
sims.cv.base.1  <- rmh.ppm( ppm16.baseline, nsim = 1000, w = o1.15, verbose = TRUE, progress = TRUE )

# Fixing naming error
# tempdat1 <- sims.cv.ppm16.1
# tempdat2 <- sims.cv.base.1
# 
# sims.cv.base.1 <- tempdat1
# sims.cv.ppm16.1 <- tempdat2
# 
# save(sims.cv.ppm16.1, file="sims.cv.ppm16.1.RData")
# save(sims.cv.base.1, file="sims.cv.base.1.RData")

dir.qscheme <- quadscheme(pppPriorTo16, method="dirichlet", exact=TRUE)
dir.areas <- dirichletAreas(dir.qscheme)

# function 
# count points in polygon[i] 

# look up ways to count points in each tile and store to individual tessellations


i=1
n= 1000

tess.counts1 <- quadratcount(sims.ppm16.1[[1]], tess = dirich16)

i=1 # start value
n= 1000 # simulations
tess.counts <- matrix(data = NA, nrow=1000, ncol = 4344)

while(i <= n){
  tess.counts[i,] <- as.vector(as.numeric(quadratcount( sims.ppm16.1[[i]] , tess = dirich16)))
  i = i+1
  if(i%%50 == 0) { cat( "Iteration", i, " \n");}
}

save(tess.counts, file="tess.counts.RData")

tess.counts <- as.data.frame(tess.counts)
tess.counts.avg <- lapply(tess.counts, mean)
tess.counts.sd <- lapply(tess.counts, sd)

hist((unlist(tess.counts.avg)), main="Histogram of Tile Simulation \n Count Averages", breaks=30, xlab="Tile Count Averages")
hist(unlist(tess.counts.sd), main="Histogram of Tile Simulations \n Count Standard Deviations", breaks=30, xlab = "Tile Count Standard Deviations")

pal.rdbu <- brewer.pal(n = 10, name = "RdBu")
plot( dirich16, values = (1 - unlist(tess.counts.avg)), do.col=TRUE, col = colourmap( pal.rdbu, range = c(-5,5) ) )
plot( dirich16, values = (log(unlist(tess.counts.avg)+.01)), do.col=TRUE, col = colourmap( pal.rdbu, range = c(-2,2) ) )
plot( dirich16, values = (1 - unlist(tess.counts.avg)), do.col=TRUE, col = colourmap( pal.rdbu, breaks= c(-5,-4,-3,-2,-1,0,.2,.4,.6,.8,1) ) )
# blue means model underestimates the counts (positive residual), red means the model overestimates the counts

# match index of avg and sd to appropriate tiles in tessellation object and plot it

# ppp(x = sims.ppm16.1[[i]]$x, y = sims.ppm16.1[[i]]$y

# ppm16.baseline residuals
i=1 # start value
n= 1000 # simulations
tess.counts16.base.1 <- matrix(data = NA, nrow=1000, ncol = 4344)

while(i <= n){
  tess.counts16.base.1[i,] <- as.vector(as.numeric(quadratcount( sims.ppm16.base.1[[i]] , tess = dirich16)))
  i = i+1
  if(i%%50 == 0) { cat( "Iteration", i, " \n");}
}

save(tess.counts16.base.1, file="tess.counts16.base.1.RData")

tess.counts16.base.1 <- as.data.frame(tess.counts16.base.1)
tess.counts16.base.1.avg <- lapply(tess.counts16.base.1, mean)
tess.counts16.base.1.sd <- lapply(tess.counts16.base.1, sd)

hist((unlist(tess.counts16.base.1.avg)), main="Histogram of Tile Simulation \n Count Averages", breaks=30, xlab="Tile Count Averages")
hist(unlist(tess.counts16.base.1.sd), main="Histogram of Tile Simulations \n Count Standard Deviations", breaks=30, xlab = "Tile Count Standard Deviations")

pal.rdbu <- brewer.pal(n = 10, name = "RdBu")
plot( dirich16, values = (1 - unlist(tess.counts16.base.1.avg)), do.col=TRUE, col = colourmap( pal.rdbu, range = c(-5,5) ) )
plot( dirich16, values = (log(unlist(tess.counts16.base.1.avg)+.01)), do.col=TRUE, col = colourmap( pal.rdbu, range = c(-2,2) ) )
plot( dirich16, values = (1 - unlist(tess.counts16.base.1.avg)), do.col=TRUE, col = colourmap( pal.rdbu, breaks= c(-5,-4,-3,-2,-1,0,.2,.4,.6,.8,1) ) , main="")
# blue means model underestimates the counts (positive residual), red means the model overestimates the counts


# cross validation dirichlet calculation
dirich17 <- dirichlet(pppPriorTo17) 

save(dirich17, file="dirich17.RData")

# ppm16.1.cv - expanded model residuals
i=1 # start value
n= 1000 # simulations
tess.counts.cv.ppm16.1 <- matrix(data = NA, nrow=1000, ncol = 4285) # ncol = number of tiles from dirich17

while(i <= n){
  tess.counts.cv.ppm16.1[i,] <- as.vector(as.numeric(quadratcount( sims.cv.ppm16.1[[i]] , tess = dirich17)))
  i = i+1
  if(i%%50 == 0) { cat( "Iteration", i, " \n");}
}

save(tess.counts.cv.ppm16.1, file="tess.counts.cv.ppm16.1.RData")

# plots for cv expanded model

# ppm16.base.1.cv  -  baseline residuals
i=1 # start value
n= 1000 # simulations
tess.counts.cv.base.1 <- matrix(data = NA, nrow=1000, ncol = 4285)

while(i <= n){
  tess.counts.cv.base.1[i,] <- as.vector(as.numeric(quadratcount( sims.cv.base.1[[i]] , tess = dirich17)))
  i = i+1
  if(i%%50 == 0) { cat( "Iteration", i, " \n");}
}

save(tess.counts.cv.base.1, file="tess.counts.cv.base.1.RData")


# basic plots for CV models 

# setwd("D:/Thesis Paper/")
# load("dirich17.areas.RData")
# load("dirich17.RData")
# load("tess.counts.cv.base.1.RData")
# load("tess.counts.cv.ppm16.1.RData")


tess.counts.cv.base.1 <- as.data.frame(tess.counts.cv.base.1)
tess.counts.cv.ppm16.1 <- as.data.frame(tess.counts.cv.ppm16.1)

tess.counts.cv.base.1.avg <- lapply(tess.counts.cv.base.1, mean)
tess.counts.cv.ppm16.1.avg <- lapply(tess.counts.cv.ppm16.1, mean)

# root of ssr of area adjusted dirichlet tiles 
sqrt(mean(( (unlist(tess.counts.cv.base.1.avg) - 1) * ( dirich17.areas ) )^2))
sqrt(mean(( (unlist(tess.counts.cv.ppm16.1.avg) - 1) * ( dirich17.areas ) )^2)) 
# proportion relative improvement of expanded model vs baseline model
1 - ( sqrt(sum(( (unlist(tess.counts.cv.ppm16.1.avg) - 1) * ( dirich17.areas ) )^2))  / sqrt(sum(( (unlist(tess.counts.cv.base.1.avg) - 1) * ( dirich17.areas ) )^2))  )


tess.counts.cv.base.1.sd <- lapply(tess.counts.cv.base.1, sd)
tess.counts.cv.ppm16.1.sd <- lapply(tess.counts.cv.ppm16.1, sd)

hist((unlist(tess.counts.cv.base.1.avg)), main="Histogram of Tile Simulation \n Count Averages - Baseline", breaks=30, xlab="Tile Count Averages")
hist((unlist(tess.counts.cv.ppm16.1.avg)), main="Histogram of Tile Simulation \n Count Averages - Expanded", breaks=30, xlab="Tile Count Averages")

# average of tile residual averages
mean( unlist(tess.counts.cv.base.1.avg) )
mean( unlist(tess.counts.cv.ppm16.1.avg) )
mean( unlist(tess.counts.cv.ppm16.1.avg) ) / mean( unlist(tess.counts.cv.base.1.avg) ) 
# 


# average of tile residual standard deviations
mean( unlist(tess.counts.cv.base.1.sd) )
mean( unlist(tess.counts.cv.ppm16.1.sd) )

hist(unlist(tess.counts.cv.base.1.sd), main="Histogram of Tile Simulations \n Count Standard Deviations - Baseline", breaks=30, xlab = "Tile Count Standard Deviations")
hist(unlist(tess.counts.cv.ppm16.1.sd), main="Histogram of Tile Simulations \n Count Standard Deviations - Expanded", breaks=30, xlab = "Tile Count Standard Deviations")


# fix the break point values
# AND FIX LEGEND!!!!!!
pal.rdbu1 <- brewer.pal(n = 10, name = "RdBu")
plot( dirich17, values = (1 - unlist(tess.counts.cv.base.1.avg)), do.col=TRUE, col = colourmap( pal.rdbu1, breaks= c(-7*(5/5),-7*(4/5),-7*(3/5),-7*(2/5),-7*(1/5),0,.2,.4,.6,.8,1) ) , main="", text.font=10)
plot( dirich17, values = (1 - unlist(tess.counts.cv.ppm16.1.avg)), do.col=TRUE, col = colourmap( pal.rdbu1, breaks= c(-7*(5/5),-7*(4/5),-7*(3/5),-7*(2/5),-7*(1/5),0,.2,.4,.6,.8,1) ) , main="")

# fix the break point values
pal.rdbu2 <- brewer.pal(n = 10, name = "RdBu")
plot( dirich17, values = (1 - unlist(tess.counts.cv.base.1.sd)), do.col=TRUE, col = colourmap( pal.rdbu, breaks= c(-5,-4,-3,-2,-1,0,.2,.4,.6,.8,1) ) , main="")
plot( dirich17, values = (1 - unlist(tess.counts.cv.ppm16.1.sd)), do.col=TRUE, col = colourmap( pal.rdbu, breaks= c(-5,-4,-3,-2,-1,0,.2,.4,.6,.8,1) ) , main="")

# blue means model underestimates the counts (positive residual), red means the model overestimates the counts


# plots for cv baseline model


# plots for cv expanded model

# FIX THE DAMN LEGENDS

# be sure to include histograms of the distribution of residuals (for both pixel and dirichilet tessellations)

# End file