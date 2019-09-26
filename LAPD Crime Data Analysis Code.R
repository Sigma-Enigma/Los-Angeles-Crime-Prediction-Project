
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


# if I had better spatial data (read: heatmaps of each of my covariates, I could do much cooler stuff and even incorporate the spatial and time variables)


######### DESCRIPTIVE STATISTICS SECTION ##########

# can improve these later with ggplot

# burglaries by age, most likely age of home owner
plot(table(sort(burgs15$Victim.Age)), ylab = "Counts", xlab = "Age", main = "Burglaries by Age")
# improve with ggplot

# burglaries by sex
plot(sort(table(burgs15$Victim.Sex), decreasing = TRUE), ylab="Counts", xlab="Sex", main="Burglaries by Sex") 

# burglaries by ethnicity, needs legend
# Descent Code: A - Other Asian B - Black C - Chinese D - Cambodian F - Filipino G - Guamanian H - Hispanic/Latin/Mexican I - American Indian/Alaskan Native J - Japanese K - Korean L - Laotian O - Other P - Pacific Islander S - Samoan U - Hawaiian V - Vietnamese W - White X - Unknown Z - Asian Indian
plot(sort(table(burgs15$Victim.Descent), decreasing = TRUE), ylab="Counts", xlab="Ethnic Descent", main="Burglary Victims by Ethnic Descent") # add a legend for ethnicities

########## PLOTTING OF BURGLARY POINTS ############



# Shapefile includes LA county zipcode boundaries + tabular data by zipcode region

# This file is for 2015 covariates
zipbounds1 <- readOGR("D:/Dropbox/Thesis Data/Thesis Code Redo Data 2/Zip code prj files 2", "CAMS_ZIPCODE_STREET_SPECIFIC")


# exctracts 
zb1.15 <- subset(zipbounds1, !duplicated(zipbounds1$ZIP_NUM_N_) )
zb1.15 <- subset(zipbounds1, (zipbounds1$ZIP_NUM_N_) %in% zl )

zl2 <- unique(zipbounds1$ZIP_NUM_N_)
zl2 <- zl2[-85]
zb2 <- subset(zipbounds1, zipbounds1$ZIP_NUM_N_ %in% zl2 )
pal3 <- c("#bdbdbd" , brewer.pal(9, "OrRd") )
spplot(zb2, "BURG_COUNT", main="2016 Annual Burglary Counts in LA County ", col.regions=pal3, at = c(0, .99, 25,50, 75, 100, 125, 150, 175, 200, 225)) 

sbs1.15 <- subset(burgs16, burgs16$Zip %in% zl) #recall points are from following year 16


o1.15 <- as.owin(zb1.15) 
pal2 <- brewer.pal(9, "OrRd")
spplot(zb1.15, "BURG_COUNT", main="2016 Annual Burglary Counts in LA County ", col.regions=pal2, cuts=8) 

# now do the same for 2016 covariates

# this file is for 2016 covariates 
zipbounds16 <- readOGR("D:/Dropbox/Thesis Data/Thesis Code Redo Data 2/Zip code prj files 3", "CAMS_ZIPCODE_STREET_SPECIFIC")


zb1.16 <- subset(zipbounds16, !duplicated(zipbounds16$ZIP_NUM_N_) )
zb1.16 <- subset(zipbounds16, (zipbounds16$ZIP_NUM_N_) %in% zl )


# fix sbs1.16 it is a duplicate of sbs 1.14
sbs1.16 <- subset(burgs17, burgs17$Zip %in% zl)


o1.16 <- as.owin(zb1.16) 
pal2 <- brewer.pal(9, "OrRd")
spplot(zb1.16, "BURG_COUNT", main="2017 Annual Burglary Counts in LA County ", col.regions=pal2, cuts=8) 

#changed values from 100 to 500
rast <- raster(ncol=500, nrow=500, ext = extent(zb1.15) )
r.pop <- rasterize(zb1.15, rast, field = zb1.15@data$TOT_POP, fun = "mean", 
                   update = TRUE, updateValue = "NA")
plot(r.pop, main = "Zip Code Region Population Estimate" )

r.area.ziptotal <- rasterize(zb1.15, rast, field = zb1.15@data$ZIP_TOT_AR/2.788e+7, fun = "mean", 
                             update = TRUE, updateValue = "NA")
plot(r.area.ziptotal, main = "Total Area of Zip Code Region")

r.area.capita <- rasterize(zb1.15, rast, field = (zb1.15@data$ZIP_TOT_AR/2.788e+7)/zb1.15@data$TOT_POP, fun = "mean", 
                           update = TRUE, updateValue = "NA")
plot(r.area.ziptotal, main = "Square Mile per Capita")


r.pop.d <- rasterize(zb1.15, rast, field = ((zb1.15@data$TOT_POP*(1/zb1.15@data$ZIP_TOT_AR))*2.788e+7) , fun = "mean", 
                     update = TRUE, updateValue = "NA")
plot(r.pop.d, main = " Zip Code Region Population Density Per Square Mile")

r.agi1 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_AG, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi1, main = "Percent of Returns with AGI < $25,000")

r.agi2 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_A2, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi2, main = " * * Percent of Returns with AGI > $25,000 & < $50,000" )

r.agi3 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_A3, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi3, main = " * * Percent of Returns with AGI > $50,000 & < $75,000" )

r.agi4 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_A4, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi4, main = " * * Percent of Returns with AGI > $75,000 & < $100,000" )

r.agi5 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_A5, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi5, main = " * * Percent of Returns with AGI > $100,000 & < $200,000" )

r.agi6 <- rasterize(zb1.15, rast, field = zb1.15@data$PERCENT_A6, fun = "mean", 
                    update = TRUE, updateValue = "NA")
plot(r.agi6, main = " * * Percent of Returns with AGI > $200,000" )

r.house <- rasterize(zb1.15, rast, field = (zb1.15@data$X2015_HOUS) , fun = "mean", 
                     update = TRUE, updateValue = "NA")
plot(r.house, main = "Number of Housing Units")

r.house.d <- rasterize(zb1.15, rast, field = ((zb1.15@data$X2015_HOUS*(1/zb1.15@data$ZIP_TOT_AR))*2.788e+7) , fun = "mean", 
                       update = TRUE, updateValue = "NA")
plot(r.house.d, main = "Average Housing Density per Square Mile")

r.med.income <- rasterize(zb1.15, rast, field = (zb1.15@data$X2015_MEDI) , fun = "mean", 
                          update = TRUE, updateValue = "NA")
plot(r.med.income, main = "Median Income")

r.pov.allfams <- rasterize(zb1.15, rast, field = (zb1.15@data$X2015_POVE) , fun = "mean", 
                           update = TRUE, updateValue = "NA")
plot(r.pov.allfams, main = "Percentage of Famalies in Poverty")

r.pov.nohus <- rasterize(zb1.15, rast, field = (zb1.15@data$X2015_FAMI) , fun = "mean", 
                         update = TRUE, updateValue = "NA")
plot(r.pov.nohus, main = "Percentage of Single Mother Families Poverty")

r.edu.25noHS <- rasterize(zb1.15, rast, field = (zb1.15@data$X25__NO_DI) , fun = "mean", 
                          update = TRUE, updateValue = "NA")
plot(r.edu.25noHS, main = "Number of people age 25+ without HS Diploma")

r.edu.25noHS.percent <- rasterize(zb1.15, rast, field = (zb1.15@data$X25__NO_DI/zb1.15@data$TOT_POP) , fun = "mean", 
                                  update = TRUE, updateValue = "NA")
plot(r.edu.25noHS.percent, main = "Percentage of people age 25+ without HS Diploma")


r.edu.18noHS <- rasterize(zb1.15, rast, field = (zb1.15@data$X18_24_NO_) , fun = "mean", 
                          update = TRUE, updateValue = "NA")
plot(r.edu.18noHS, main = "Number of people age 18-24 without HS Diploma")

r.edu.18noHS.percent <- rasterize(zb1.15, rast, field = (zb1.15@data$X18_24_NO_/zb1.15@data$TOT_POP) , fun = "mean", 
                                  update = TRUE, updateValue = "NA")
plot(r.edu.18noHS.percent, main = "Percentage of people age 18-24 without HS Diploma")

# IMPORTANT! : any values with raw number of people/housing units may need to be corrected for total population (divided by totpop)


# Be sure to include an interaction term for area/capita ...done

im.a1 <- as.im(r.agi1) # key command works with classes: owins, matrix, im, tess, etc
im.a2 <- as.im(r.agi2)
im.a3 <- as.im(r.agi3)
im.a4 <- as.im(r.agi4)
im.a5 <- as.im(r.agi5)
im.a6 <- as.im(r.agi6)

im.area.ziptot <- as.im(r.area.ziptotal)

im.edu.18noHS <- as.im(r.edu.18noHS)
im.edu.25noHS <-  as.im(r.edu.25noHS)

im.edu.18noHS.percent <- as.im(r.edu.18noHS.percent)
im.edu.25noHS.percent <- as.im(r.edu.25noHS.percent)

im.house <- as.im(r.house)
im.house.d <- as.im(r.house.d)

im.med.income <- as.im(r.med.income)

im.pop <- as.im(r.pop)
im.pop.d <- as.im(r.pop.d) 

im.pov.allfams <- as.im(r.pov.allfams)
im.pov.nohus <- as.im(r.pov.nohus)

##### aside: projecting burglarly locations of 2015 onto current owindow. Used to create density estimate of prior years burgs (as an image)
zb1.14 <- subset(zipbounds1, !duplicated(zipbounds1$ZIP_NUM_N_) )
zb1.14 <- subset(zipbounds1, (zipbounds1$ZIP_NUM_N_) %in% zl )

sbs1.14 <- subset(burgs15, burgs15$Zip %in% zl) #recall points are from following year 16

coordinates(sbs1.14) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(sbs1.14) <- CRS("+proj=longlat +ellps=WGS84")
sbs1.14@proj4string

sbs1.14 <- spTransform(sbs1.14, proj4string(zb1.14))


# NOTE JITTER here destroys exact duplicates, this pattern is repeated for point patterns of all years
# year 1: 1371 of 7415
# year 2: 718  of 4526
# year 3: 709  of 4546
# year 4: 657  of 4362
# year 5: 734  of 4300

# may need to change sbs1.14$Long to sbs1.14@coords[,1]

pppPriorTo15 <- ppp(x = jitter(sbs1.14@coords[,1]), y = jitter(sbs1.14@coords[,2]), window = o1.15 )

pppPriorTo15 <- rescale(pppPriorTo15, (5280) )

bw.pplPriorto15 <- bw.ppl(pppPriorTo15) # leave one out bandwidth estimate for KDE

# MAKE LARGER BANDWIDTH (bw) ESTIMATES WITH EPANECHNIKOV KERNELS AND OTHER ESTIMATION METHODS!!

##### end aside



im.burg.dens15 <- density(pppPriorTo15, sigma=bw.pplPriorto15, diggle=TRUE, positive=TRUE, dimyx = c(500,500))

### START ppp for 2014 year

zb1.13 <- subset(zipbounds1, !duplicated(zipbounds1$ZIP_NUM_N_) )
zb1.13 <- subset(zipbounds1, (zipbounds1$ZIP_NUM_N_) %in% zl )

sbs1.13 <- subset(burgs14, burgs14$Zip %in% zl) #recall points are from following year 16

coordinates(sbs1.13) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(sbs1.13) <- CRS("+proj=longlat +ellps=WGS84")
sbs1.13@proj4string

sbs1.13 <- spTransform(sbs1.13, proj4string(zb1.13))
# may need to change sbs1.14$Long to sbs1.14@coords[,1]

pppPriorTo14 <- ppp(x = jitter(sbs1.13@coords[,1]), y = jitter(sbs1.13@coords[,2]), window = o1.15orig )

pppPriorTo14 <- rescale(pppPriorTo14, (5280) )

bw.pplPriorto14 <- bw.ppl(pppPriorTo14) # leave one out bandwidth estimate for KDE

im.burg.dens14 <- density(pppPriorTo14, sigma=bw.pplPriorto14, diggle=TRUE, positive=TRUE, dimyx = c(500,500))
im.burg.dens14.1 <- density(pppPriorTo14, diggle=TRUE, leaveoneout=TRUE, positive=TRUE, dimyx = c(500,500))
im.burg.dens14.2 <- density(pppPriorTo14, sigma=bw.pplPriorto14, diggle=TRUE, leaveoneout=FALSE, positive=TRUE, dimyx = c(500,500))


### END ppp for 2014 burgs

### START pp for 2013 burgs
zb1.12 <- subset(zipbounds1, !duplicated(zipbounds1$ZIP_NUM_N_) )
zb1.12 <- subset(zipbounds1, (zipbounds1$ZIP_NUM_N_) %in% zl )

#sbs1.12 <- subset(burgs13, burgs13$Zip %in% zl) #recall points are from following year 16
sbs1.12 <- burgs13

coordinates(sbs1.12) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(sbs1.12) <- CRS("+proj=longlat +ellps=WGS84")
sbs1.12@proj4string

sbs1.12 <- spTransform(sbs1.12, proj4string(zb1.12))

pppPriorTo13 <- ppp(x = jitter(sbs1.12@coords[,1]), y = jitter(sbs1.12@coords[,2]), window = o1.15orig )

pppPriorTo13 <- rescale(pppPriorTo13, (5280) )

bw.pplPriorto13 <- bw.ppl(pppPriorTo13) # leave one out bandwidth estimate for KDE

im.burg.dens13 <- density(pppPriorTo13, sigma=bw.pplPriorto13, diggle=TRUE, positive=TRUE, dimyx = c(500,500))

### END ppp for 2013 burgs



# stores locations and sets projection (in R shapefile format)
coordinates(sbs1.15) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(sbs1.15) <- CRS("+proj=longlat +ellps=WGS84")
sbs1.15@proj4string


sbs1.15 <- spTransform(sbs1.15, proj4string(zb1.15))
plot(zb1.15, main="Burglary Locations")
points(sbs1.15, pch=".", col="red" )
# few points outside the boundaries...
# way to find values outside of owin=o1.15?


### SECOND ASIDE

o1.15orig <- rescale(o1.15, (1/5280) )

pppPriorTo16 <- ppp(x = jitter(sbs1.15@coords[,1]), y = jitter(sbs1.15@coords[,2]), window= o1.15orig )

pppPriorTo16 <- rescale(pppPriorTo16, (5280) )

bw.pplPriorto16 <- bw.ppl(pppPriorTo16)

# leave one out density of 2016 sfd burglaries
im.burg.dens16 <- density(pppPriorTo16, sigma=bw.pplPriorto16, diggle=TRUE, positive=TRUE, dimyx = c(500,500))

## set sbs1.16 to a spatial points dataset
# zb1.16 <- subset(zipbounds1, !duplicated(zipbounds1$ZIP_NUM_N_) )
# zb1.16 <- subset(zipbounds1, (zipbounds1$ZIP_NUM_N_) %in% zl )

# sbs1.15 <- subset(burgs16, burgs16$Zip %in% zl) #recall points are from following year 16


coordinates(sbs1.16) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(sbs1.16) <- CRS("+proj=longlat +ellps=WGS84")
sbs1.16@proj4string

sbs1.16 <- spTransform(sbs1.16, proj4string(zb1.16))

pppPriorTo17 <- ppp(x = jitter(sbs1.16@coords[,1]), y = jitter(sbs1.16@coords[,2]), window= o1.15orig )

pppPriorTo17 <- rescale(pppPriorTo17, (5280) )

bw.pplPriorto17 <- bw.ppl(pppPriorTo17)

bw.scottPriorto17 <- bw.scott(pppPriorTo17)

im.burg.dens17 <- density(pppPriorTo17, sigma=bw.pplPriorto17, diggle=TRUE, positive=TRUE, dimyx = c(500,500))

plot(im.burg.dens17)
plot(im.burg.dens17.2)

# average ppl bandwidth
bw.ppl.avg <- (bw.pplPriorto13 + bw.pplPriorto14 + bw.pplPriorto15 + bw.pplPriorto16 + bw.pplPriorto17 ) /5

#
r.pop.16 <- rasterize(zb1.16, rast, field = zb1.16@data$TOT_POP, fun = "mean", 
                      update = TRUE, updateValue = "NA")

r.pop.d.16 <- rasterize(zb1.16, rast, field = ((zb1.16@data$TOT_POP*(1/zb1.16@data$ZIP_TOT_AR))*2.788e+7) , fun = "mean", 
                        update = TRUE, updateValue = "NA")

r.agi1.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_AG, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.agi2.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_A2, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.agi3.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_A3, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.agi4.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_A4, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.agi5.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_A5, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.agi6.16 <- rasterize(zb1.16, rast, field = zb1.16@data$PERCENT_A6, fun = "mean", 
                       update = TRUE, updateValue = "NA")

r.house.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X2016_HOUS) , fun = "mean", 
                        update = TRUE, updateValue = "NA")

r.house.d.16 <- rasterize(zb1.16, rast, field = ((zb1.16@data$X2016_HOUS*(1/zb1.16@data$ZIP_TOT_AR))*2.788e+7) , fun = "mean", 
                          update = TRUE, updateValue = "NA")

r.med.income.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X2016_MEDI) , fun = "mean", 
                             update = TRUE, updateValue = "NA")

r.pov.allfams.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X2016_POVE) , fun = "mean", 
                              update = TRUE, updateValue = "NA")

r.pov.nohus.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X2016_FAMI) , fun = "mean", 
                            update = TRUE, updateValue = "NA")

r.edu.25noHS.percent.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X25__NO_DI/zb1.16@data$TOT_POP) , fun = "mean", 
                                     update = TRUE, updateValue = "NA")

r.edu.18noHS.percent.16 <- rasterize(zb1.16, rast, field = (zb1.16@data$X18_24_NO_ / zb1.16@data$TOT_POP ) , fun = "mean", 
                                     update = TRUE, updateValue = "NA") 


im.a1.y16 <- as.im(r.agi1.16) # key command works with classes: owins, matrix, im, tess, etc
im.a2.y16 <- as.im(r.agi2.16)
im.a3.y16 <- as.im(r.agi3.16)
im.a4.y16 <- as.im(r.agi4.16)
im.a5.y16 <- as.im(r.agi5.16)
im.a6.y16 <- as.im(r.agi6.16)


im.edu.18noHS.percent.y16 <- as.im(r.edu.18noHS.percent.16)
im.edu.25noHS.percent.y16 <- as.im(r.edu.25noHS.percent.16)

im.house.y16 <- as.im(r.house.16)
im.house.d.y16 <- as.im(r.house.d.16)

im.med.income.y16 <- as.im(r.med.income.16)

im.pop.y16 <- as.im(r.pop.16)
im.pop.d.y16 <- as.im(r.pop.d.16) 

im.pov.allfams.y16 <- as.im(r.pov.allfams.16)
im.pov.nohus.y16 <- as.im(r.pov.nohus.16)

# recall all covariates were rescaled to improve accuracy

im.a1.y16 <- rescale.im(im.a1.y16, s=5280, unitname="miles")
im.a2.y16 <- rescale.im(im.a2.y16, s=5280, unitname="miles")
im.a3.y16 <- rescale.im(im.a3.y16, s=5280, unitname="miles")
im.a4.y16 <- rescale.im(im.a4.y16, s=5280, unitname="miles")
im.a5.y16 <- rescale.im(im.a5.y16, s=5280, unitname="miles")
im.a6.y16 <- rescale.im(im.a6.y16, s=5280, unitname="miles")
im.edu.18noHS.percent.y16  <- rescale.im(im.edu.18noHS.percent.y16, s=5280, unitname="miles")
im.edu.25noHS.percent.y16  <- rescale.im(im.edu.25noHS.percent.y16, s=5280, unitname="miles")
im.house.y16   <- rescale.im(im.house.y16, s=5280, unitname="miles")
im.house.d.y16 <- rescale.im(im.house.d.y16, s=5280, unitname="miles")
im.med.income.y16  <- rescale.im(im.med.income.y16, s=5280, unitname="miles")
im.pop.y16  <- rescale.im(im.pop.y16, s=5280, unitname="miles")
im.pop.d.y16  <- rescale.im(im.pop.d.y16, s=5280, unitname="miles")
im.pov.allfams.y16  <- rescale.im(im.pov.allfams.y16, s=5280, unitname="miles")
im.pov.nohus.y16  <- rescale.im(im.pov.nohus.y16, s=5280, unitname="miles")





im.data <- list(burg.dens15=im.burg.dens16,  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )

preds1 <- predict.ppm(ppm.reduced1, window=o1.15, covariates = im.data, ngrid=500, type="trend", interval = "confidence")

### END SECOND ASIDE

# ppp1.15 <- ppp(x = jitter(sbs1.15@coords[,1]), y = jitter(sbs1.15@coords[,2]), window = o1.15 )
# loading same randomized ppp for consistency
load(file="ppp1.15.RData")

# check to see if ppx function has been updated!


# NOTE NO MARKS WERE USED IN pp1.15 MODEL THUS FALSE TIME DUPLICATES MAY EXIST
# review documentation to make sure what sytax for model is appropriate
sum(duplicated.ppp(ppp1.15)) # marks must also be duplicates
sum(duplicated.ppp(ppp1.15, rule="deldir")) # number of spatial AND temporal duplicates
sum(duplicated.ppp(ppp1.15, rule="unmark")) # true number of spatial duplicates

which( duplicated.ppp(ppp1.15) == TRUE)


# code to show location of duplicate points
plot(zb1.15)
points(ppp1.15[which( duplicated.ppp(ppp1.15, rule="unmark") == TRUE)], col="red", pch = 1) # duplicate location only
points(ppp1.15[which( duplicated.ppp(ppp1.15) == TRUE)], col="green") # duplicate location and time


ppp0.15 <- ppp1.15
# testing jitter variance

head( jitter(ppp1.15$x, factor = .1 )) - head(ppp1.15$x)
range( jitter(ppp1.15$x, factor = .1 ) - ppp1.15$x )
mean( jitter(ppp1.15$x, factor = .1 ) - ppp1.15$x )
sd( jitter(ppp1.15$x, factor = .1 ) - ppp1.15$x )


# add jitter to destroy duplicates
ppp1.15$x <- jitter(ppp1.15$x)
ppp1.15$y <- jitter(ppp1.15$y)


# Order of analysis
# CSR check
# Homogenous ppp # see pg 246 for diagnostic test of inhom kest (to check for clustering via L function)
# Inhomogenous ppp
# Clustered pp # 456 for example of LGCP simulation example; clusterradius(), rNeymanScott(), rThomas(), rVarGamma(), rMatClust()

# After checking each of these, use diagnostic plots to determine which fit best

# Once you have the best model type (of above versions) add in covariates sequentially and check improvement of fit using ANOVA/hypothesis tests
# try adding a density plot of the previous years burgs on my current owin (to get a general idea of the crime surface area)


# Finally once you settle on a model use the multiple covariate model and the population density only model to predict the following years data.
# be sure that the tax data is of the correct year, 

Gest(ppp1.15)
plot(Gest(ppp1.15))

kest1 <- Kest(ppp1.15, correction="all", var.appro=TRUE, ratio=TRUE)
save(kest1, file="kest1.RData")
plot(kest1)

lambda1 <- density(ppp1.15, bw.ppl)

kinhom1 <- Kinhom(ppp1.15, lambda1, correction="all")

# chpts 6-7, 9-13, 16

kest2 <- Kest(ppmfull3)

envelope(ppp1.15)

#use hypothesis test between poisson and inhomogenous poisson models ()

# NOTE in our ppm, ppp1.15 = points of 2016 year!!! 
ppm1 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop, covariates = list(pop=im.pop), subset = o1.15 )
summary(ppm1)
plot(ppm1) # only raw population values


# re-read ppm documentation and model UPDATE TO GITHUB VERSION TOO

ppm1.1 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop.d, covariates = list(pop.d=im.pop.d), subset = o1.15 )
summary(ppm1.1)
plot(ppm1.1) # only population density values



ppm2 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop + a1 + a2 + a3 + a4 + a5 + a6, covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6) ) 
summary(ppm2)
plot(ppm2) # raw population  + tax data

ppm2.1 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop.d + a1 + a2 + a3 + a4 + a5 + a6, covariates = list(pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6) ) 
summary(ppm2.1)
plot(ppm2.1) # population density + tax data

ppm3.1 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop.d + a1 + a2 + a3 + a4 + a5 + a6, covariates = list(pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6) ) 
summary(ppm3.1)

# remember any total count values may need an interaction terms with area.ziptot & pop & house
ppmfull <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop + a1 + a2 + a3 + a4 + a5 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS + edu.25noHS , covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS=im.edu.18noHS, edu.25noHS=im.edu.25noHS), subset = o1.15) 
summary


#try a few point interaction types
ppmfull2.1 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop + a1 + a2 + a3 + a4 + a5 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS + edu.25noHS , covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS=im.edu.18noHS, edu.25noHS=im.edu.25noHS), subset = o1.15) 

#also try alternate variables which are a nonadditive function of two variables
ppmfull3 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop + a1 + a2 + a3 + a4 + a5 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS.percent + edu.25noHS.percent , covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS.percent=im.edu.18noHS.percent, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15) 

diagnose1 <- diagnose.ppm(ppmfull3)

#plot(envelope(ppmfull3, verbose=TRUE, nsim=10))

ppmfull4 <- ppm( ppp(x = ppp1.15$x, y = ppp1.15$y, window = o1.15)  ~ pop + a1 + a2 + a3 + a4 + a5 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS.percent + edu.25noHS.percent , covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS.percent=im.edu.18noHS.percent, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15) 


kppmfull1 <- kppm( ppp1.15 ~  pop + a1 + a2 + a3 + a4 + a5 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS.percent + edu.25noHS.percent , covariates = list(pop=im.pop, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS.percent=im.edu.18noHS.percent, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


kppmtest1 <- kppm( ppp1.15 ~  pop.d + med.income + pov.allfams + edu.25noHS.percent , covariates = list(pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), cluster="VarGamma", subset = o1.15, rmax= 15000)

#### Final set of ppm functions





covs1 <- lapply( X = list(im.a1,
                          im.a2
                          , im.a3
                          , im.a4
                          , im.a5
                          , im.a6
                          , im.area.ziptot
                          , im.edu.18noHS 
                          , im.edu.25noHS 
                          , im.edu.18noHS.percent 
                          , im.edu.25noHS.percent 
                          , im.house 
                          , im.house.d
                          , im.med.income 
                          , im.pop 
                          , im.pop.d 
                          , im.pov.allfams 
                          , im.pov.nohus 
                          
),
FUN= rescale.im, s=(5280), unitname="miles")

im.a1 <- rescale.im(im.a1, s=5280, unitname="miles")
im.a2 <- rescale.im(im.a2, s=5280, unitname="miles")
im.a3 <- rescale.im(im.a3, s=5280, unitname="miles")
im.a4 <- rescale.im(im.a4, s=5280, unitname="miles")
im.a5 <- rescale.im(im.a5, s=5280, unitname="miles")
im.a6 <- rescale.im(im.a6, s=5280, unitname="miles")
im.area.ziptot <- rescale.im(im.area.ziptot, s=5280, unitname="miles")
im.edu.18noHS  <- rescale.im(im.edu.18noHS, s=5280, unitname="miles")
im.edu.25noHS  <- rescale.im(im.edu.25noHS, s=5280, unitname="miles")
im.edu.18noHS.percent  <- rescale.im(im.edu.18noHS.percent, s=5280, unitname="miles")
im.edu.25noHS.percent  <- rescale.im(im.edu.25noHS.percent, s=5280, unitname="miles")
im.house   <- rescale.im(im.house, s=5280, unitname="miles")
im.house.d <- rescale.im(im.house.d, s=5280, unitname="miles")
im.med.income  <- rescale.im(im.med.income, s=5280, unitname="miles")
im.pop  <- rescale.im(im.pop, s=5280, unitname="miles")
im.pop.d  <- rescale.im(im.pop.d, s=5280, unitname="miles")
im.pov.allfams  <- rescale.im(im.pov.allfams, s=5280, unitname="miles")
im.pov.nohus  <- rescale.im(im.pov.nohus, s=5280, unitname="miles")

ppp1.15 <- rescale(ppp1.15, (5280) )
o1.15 <- rescale(o1.15, (5280) )

ppm0 <- ppm( ppp1.15  ~ burg.dens15, covariates = list(burg.dens15=im.burg.dens15), subset = o1.15 )


ppm.reduced1 <-  ppm( ppp1.15 ~ burg.dens15 + pop.d + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


ppm.reduced2 <-  ppm( ppp1.15 ~ burg.dens15 + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


ppm.reduced1.mult <-  ppm( ppp1.15 ~ burg.dens15 * pop.d * med.income * pov.allfams * edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)

ppm.reduced1.mult.1 <-  ppm( ppp1.15 ~ burg.dens15 + pop.d + med.income + pov.allfams + edu.25noHS.percent + burg.dens15:pov.allfams + burg.dens15:edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)

ppm.reduced1.mult.2 <-  ppm( ppp1.15 ~ burg.dens15 + pop.d + med.income + pov.allfams + edu.25noHS.percent + burg.dens15:med.income , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


ppm.reduced2.mult <-  ppm( ppp1.15 ~ burg.dens15 * pop.d * a1 * a2 * a3 * a4 * a6 * med.income * pov.allfams * edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)



ppm.all <- ppm( ppp1.15 ~ burg.dens15 + pop.d + a1 + a2 + a3 + a4 + a6 + house + area.ziptot + med.income + pov.allfams + pov.nohus + edu.18noHS + edu.25noHS , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a5=im.a5, a6=im.a6, house=im.house, area.ziptot=im.area.ziptot, med.income=im.med.income, pov.allfams=im.pov.allfams, pov.nohus=im.pov.nohus, edu.18noHS=im.edu.18noHS, edu.25noHS=im.edu.25noHS), subset = o1.15) 

# did i use the wrong ppp above? should it not have been ppp1.16
# note the intensity of the model is astronomical


# quick and dirty models

env.e.ppm0.kinhom <-  envelope(ppm0, Kinhom, lambda=ppm0,  nsim=500, verbose=TRUE, global=TRUE, clipdata=TRUE, savefuns=TRUE, savepatterns=TRUE, alternative="two.sided" , correction="border")



# full runs
env.ppm0.kinhom1 <- envelope(ppm0, Kinhom, lambda=ppm0,  nsim=20, verbose=TRUE, global=TRUE, clipdata=TRUE, savefuns=TRUE, savepatterns=TRUE, alternative="two.sided" , correction="good")
env.ppm0.lest <- envelope(ppm0, Lest , lambda=ppm0,  nsim=20, verbose=TRUE, global=TRUE, clipdata=TRUE, savefuns=TRUE, savepatterns=TRUE, alternative="two.sided",  correction="border")

kppm0.1 <- kppm( ppp1.15 ~ burg.dens15, covariates = list(burg.dens15=im.burg.dens15), cluster="VarGamma", subset = o1.15)

save(kppm0.1, file="kppm0.1.RData")

kppm.reduced1.1
save(kppm.reduced1.1, file="kppm.reduced1.1.RData")


kppm0.2 

kppm.reduced1.2
pkppmtest1 <- predict.kppm(kppmtest1)

# find a way to do predictions using new covariate data and check for best fit
save(pkppmtest1, file="pkppmtest1.RData")



# first use envelope?/Kest? function to prove there is not CSR, 
# then try to use a variety of fancier models and check them



# next try a gibbs point process model (with attraction term)
# hardcore model/ softcore model
# hybrid interaction may be the most useful

# or clustered point process kppm



# Then:
# next thing use this function with new data to make predictions
# need to feed it covariate data for following year



predict.ppm()

# use nndist function as a way to check the goodness of fit of predicted points?

# continue here 6/18/18


### other stuff

burgs.all <- as.data.frame(matrix(rep(NA, 33403*2), nrow=33403, ncol=2 ))
names(burgs.all) <- c("Long", "Lat")

burgs.all$Long <- c(burgs13$Long, burgs$Long) # here burgs$Long is all years 2014-2017
burgs.all$Lat <- c(burgs13$Lat, burgs$Lat)


coordinates(burgs.all) <- ~ Long + Lat # sets CRS via sp package and converts to spatial points dataframe
proj4string(burgs.all) <- CRS("+proj=longlat +ellps=WGS84")
burgs.all@proj4string

burgs.all <- spTransform(burgs.all, proj4string(zb1.14))

ppp.all <- ppp(x= jitter(burgs.all@coords[,1]), y= jitter(burgs.all@coords[,2]), window = o1.15orig  )
ppp.all <- rescale(ppp.all, (5280) )

bw.ppl.all <- bw.ppl(ppp.all)
im.burg.dens.all.ppl <- density(ppp.all, sigma=bw.ppl(ppp.all), diggle=TRUE, positive=TRUE, dimyx = c(500,500))
plot(im.burg.dens.all.ppl)

im.burg.dens.all <- density(ppp.all, sigma=bw.ppl.all*5, diggle=TRUE, positive=TRUE, dimyx = c(500,500))
plot(im.burg.dens.all)

cust.jitter <- function(X, a){
  
  X <- runif(length(X), -a, a) + X
  return(X)
}
ppp.all.u <- ppp(x= cust.jitter(burgs.all@coords[,1]), y= jitter(burgs.all@coords[,2]), window = o1.15orig  )

#### MAY NEED TO FIX THE BURGS TO BURGS.ALL ######
plot( 
  x= c( (sort(burgs$Long[burgs$Zip == 90003], decreasing=F)[1:10]),
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=F)[1:10]), 
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=T)[1:10]),
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=T)[1:10]) 
  ),
  
  y= c( (sort(burgs$Long[burgs$Zip == 90003], decreasing=T)[1:10]), 
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=F)[1:10]), 
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=T)[1:10]), 
        (sort(burgs$Long[burgs$Zip == 90003], decreasing=F)[1:10]) 
  ), 
  pch=1, col="red" ) 


ppm.reduced2.mult.offset <-  ppm( ppp16 ~ offset(log(burg.dens15)) * pop.d * a1 * a2 * a3 * a4 * a6 * med.income * pov.allfams * edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


ppm16.reduced1.offset <-  ppm( ppp16 ~ offset(log(burg.dens15)) + pop.d + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)

ppm16.reduced2.offset <-  ppm( ppp16 ~ offset(log(burg.dens15)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens15=im.burg.dens15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


preds2 <- predict.ppm(ppm16.reduced1.offset, window=o1.15, covariates = im.data, ngrid=500, type="trend", interval = "confidence")
preds3 <- predict.ppm(ppm16.reduced2.offset, window=o1.15, covariates = im.data, ngrid=500, type="trend", interval = "confidence")


# thing to try, instead of only using prior year as baseline, use multiple years (to reduce overfitting to prior years trend)
# Caveat: If you do this you need to understand how different bandwidths for KDE may cause differences in density plots, also need to find a way to reduce the resultant density to adjust for multiple years instead of just 1


# figure out how to compare residuals of two pixel images
plot(im.burg.dens17 - preds2)
sum((im.burg.dens17 - preds2)^2)
# 8,418,408
# sum of squares of residuals shows the smaller model is better, but is it statistically different enough?
sum(abs(im.burg.dens17 - preds2))


plot(im.burg.dens17 - preds3)
sum((im.burg.dens17 - preds3)^2)
# 8,602,255
sum(abs(im.burg.dens17 - preds3))



# note each density map was constructed with various optimal bandwidths (via bw.ppl) if its important to keep constant then each im.burg.dens needs to be updated
im.burg.avg.13to15 <- (im.burg.dens13 + im.burg.dens14 + im.burg.dens15)/3
im.burg.avg.14to16 <- (im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3

im.burg.dens.all2 <- density(ppp.all, sigma=bw.ppl.avg, diggle=TRUE, positive=TRUE, dimyx = c(500,500))


# Next things to do:
# redo ppm models with fixed bandwidth .1820758
# recheck cross validation model using avg bandwidth

#these two ppm models use the density average of the prior 3 years
ppm16.reduced1.offset.densavg <-  ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)


# PRIMARY FINAL MODEL USED!!! THREE YEAR AVERAGE OF PRIOR DENSITY WITH OFFSET AND ALL TERMS
ppm16.reduced2.offset.densavg <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)
diagnose.ppm(ppm16.reduced2.offset.densavg)


im.burg.dens17.2 <- density(pppPriorTo17, sigma=bw.ppl.avg, diggle=TRUE, positive=TRUE, dimyx = c(500,500))


im.data2 <- list(burg.dens.prior=im.burg.avg.14to16,  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )


preds.base.128 <-  predict.ppm(ppm16.baseline, window=o1.15, covariates = im.data2, ngrid=128, type="trend", se=TRUE)

preds.base.count.128 <-  predict.ppm(ppm16.baseline, window=o1.15, covariates = im.data2, ngrid=128, type="count", interval = "confidence", se=TRUE)


preds4 <- predict.ppm(ppm16.reduced1.offset.densavg, window=o1.15, covariates = im.data2, ngrid=500, type="trend")
preds5 <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = im.data2, ngrid=500, type="trend", se=TRUE)

preds5.128 <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = im.data2, ngrid=128, type="trend", se=TRUE)


preds5.confint <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                              , ngrid=500, type="trend", interval = "confidence", se=TRUE)

preds5.count <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                            , ngrid=500, type="count", interval = "confidence", se=TRUE)

preds5.count.128 <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                                , ngrid=128, type="count", interval = "confidence", se=TRUE)


preds5.ci.128 <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                             , ngrid=128, type="intensity", interval = "confidence", se=TRUE)


# spatial intensity is == spatial trend when process is a PPM
# preds5.lambda.confint <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
#                              , ngrid=500, type="intensity", interval = "confidence", se=TRUE)
# 
# preds5.lambda.estimate <- predict.ppm(ppm16.reduced2.offset.densavg, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens14 + im.burg.dens15 + im.burg.dens16)/3),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
#                                      , ngrid=500, type="intensity", interval = "none", se=TRUE)


ppm16.baseline <- ppm( ppp16 ~ offset(log(burg.dens.prior)) , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)

preds.baseline <- predict.ppm(ppm16.baseline, window=o1.15, covariates = im.data2, ngrid=500, type="trend", se=TRUE)


plot(im.burg.dens17.2 - preds4)
sum((im.burg.dens17.2 - preds4)^2)
# sum of squares of residuals shows the smaller model is 5,320,250
sum(abs(im.burg.dens17.2 - preds4))


cf <- colorRampPalette( c("navy", "blue3", "darkorchid4", "violetred3", "red2", "orangered2", "orange",  "goldenrod1", "yellow"))
pal4 <- cf(1000)

### THIS IS THE MAIN PREDICTION MODEL PLOT !!!!!!
plot(im.burg.dens17.2 - preds5$estimate, col = colourmap( col = pal4, range=c(-80,100) ))
plot(im.burg.dens17.2 - preds.baseline$estimate, col = colourmap( col = pal4, range=c(-80,100) ))
plot( (preds5$estimate - preds.baseline$estimate) , main = "")
### SMOOTHED RESIDUAL DIFFERENCES PLOT ABOVE

sqrt(sum((im.burg.dens17.2 - preds5$estimate)^2))
sqrt(mean((im.burg.dens17.2 - preds5$estimate)^2) )
# ssr = 5,237,330
sqrt(sum((im.burg.dens17.2 - preds.baseline$estimate)^2))
sqrt(mean((im.burg.dens17.2 - preds.baseline$estimate)^2) )
# ssr = 5,247,183
sum(abs(im.burg.dens17.2 - preds5$estimate))
# model with tax data is better for both absr and ssr... but is it's improvement of fit statistically significant? What test would we use to compare them? What distribution does a difference of two models intensity residuals follow? Chi squared?
### END MAIN PREDICTION PLOT


#ppm...densavg2 only uses past 2 years instead of 3 years it fits slightly better, but possibly overfitting more
ppm16.reduced2.offset.densavg2 <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=((im.burg.dens14+im.burg.dens15)/2), pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15)
diagnose.ppm(ppm16.reduced2.offset.densavg2)

preds6 <- predict.ppm(ppm16.reduced2.offset.densavg2, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens15 + im.burg.dens16)/2),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                      , ngrid=500, type="trend", se=TRUE)

preds6.confint <- predict.ppm(ppm16.reduced2.offset.densavg2, window=o1.15, covariates = list(burg.dens.prior=((im.burg.dens15 + im.burg.dens16)/2),  a1=im.a1.y16, a2=im.a2.y16, a3=im.a3.y16, a4=im.a4.y16, a5=im.a5.y16, a6=im.a6.y16, edu.18noHS.percent=im.edu.18noHS.percent.y16, edu.25noHS.percent=im.edu.25noHS.percent.y16, house=im.house.y16, house.d=im.house.d.y16, med.income=im.med.income.y16, pop=im.pop.y16, pop.d=im.pop.d.y16, pov.allfams=im.pov.allfams.y16, pov.nohus=im.pov.nohus.y16 )
                              , ngrid=500, type="trend", interval = "confidence", se=TRUE)


# Should I use actual pixel counts as intensity measure instead of smoothed intensity?
# be sure to adjust this for preds5!! the three year estimates!
# I made a better measure that uses the exact pixel rasters and not the densities

plot(im.burg.dens17.2 - preds6$estimate)
plot(sqrt((im.burg.dens17.2 - preds6$estimate)^2), main="SSR Intensities")

plot( im.burg.dens17.2 > preds6.confint$confidence$`2.5%` )
plot( im.burg.dens17.2 < preds6.confint$confidence$`97.5%` )

plot( (im.burg.dens17.2 > preds6.confint$confidence$`2.5%`) & (im.burg.dens17.2 < preds6.confint$confidence$`97.5%`) , main="Regions within 95% confidence interval averagebw")
plot( !((im.burg.dens17.2 > preds6.confint$`2.5%`) & (im.burg.dens17.2 < preds6.confint$`97.5%`)) , main="Regions NOT within 95% confidence interval averagebw")

plot( (im.burg.dens17 > preds6.confint$`2.5%`) & (im.burg.dens17 < preds6.confint$`97.5%`) , main="Regions within 95% confidence interval autobw")



qcount6 <- quadratcount.ppp(ppp17, nx)
iqcount6 <- intensity.quadratcount(qcount6)

sum((im.burg.dens17.2 - preds6)^2)
# sum of squares of residuals shows the smaller model is 5,384,012
sum(abs(im.burg.dens17.2 - preds6))

plot(influence.ppm(ppm16.reduced2.offset.densavg))

# continue by standardizing all bandwidths for density maps of each year to = bw.ppl.avg
# then change change gaussian to epanechnikov kernels
# then re-average prior.burg.density heatmaps and rerun models; test fit again

im.burg.dens.all.epnkv <- density(ppp.all, sigma=.0346, leaveoneout=TRUE, kernel="epanechnikov", diggle=TRUE, positive=TRUE, dimyx = c(500,500))
# models are not much different


#  New reduced2 model to test multiplicative model

# first build new covariates
mult.im.a1 <- im.a1 * im.burg.avg.13to15
mult.im.a2 <- im.a2 * im.burg.avg.13to15
mult.im.a3 <- im.a3 * im.burg.avg.13to15
mult.im.a4 <- im.a4 * im.burg.avg.13to15
mult.im.a6 <- im.a6 * im.burg.avg.13to15
mult.im.med.income <- im.med.income * im.burg.avg.13to15
mult.im.edu.25noHS.percent <-im.edu.25noHS.percent *  im.burg.avg.13to15
mult.im.pov.allfams <- im.burg.avg.13to15 * im.pov.allfams
mult.im.pop <- im.pop * im.burg.avg.13to15


mult.im.a1.y16 <- im.a1 * im.burg.avg.14to16
mult.im.a2.y16 <- im.a2 * im.burg.avg.14to16
mult.im.a3.y16 <- im.a3 * im.burg.avg.14to16
mult.im.a4.y16 <- im.a4 * im.burg.avg.14to16
mult.im.a6.y16 <- im.a6 * im.burg.avg.14to16
mult.im.med.income.y16 <- im.med.income * im.burg.avg.14to16
mult.im.edu.25noHS.percent.y16 <-im.edu.25noHS.percent *  im.burg.avg.14to16
mult.im.pov.allfams.y16 <- im.burg.avg.14to16 * im.pov.allfams
mult.im.pop.y16 <- im.pop.y16 * im.burg.avg.14to16

mult.
ppm.reduced2.hybrid.mult <-  ppm( ppp1.15 ~ prior.dens + pop + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( prior.dens=im.burg.avg.13to15, pop=mult.im.pop, a1=im.a1, a2=mult.im.a2, a3=mult.im.a3, a4=mult.im.a4, a6=mult.im.a6, med.income=mult.im.med.income, pov.allfams=mult.im.pov.allfams, edu.25noHS.percent=mult.im.edu.25noHS.percent), subset = o1.15)

im.data3 <- list(prior.dens=im.burg.avg.14to16,  a1=mult.im.a1.y16, a2=mult.im.a2.y16, a3=mult.im.a3.y16, a4=mult.im.a4.y16, a6=mult.im.a6.y16, edu.25noHS.percent=mult.im.edu.25noHS.percent.y16, med.income=mult.im.med.income.y16, pop=mult.im.pop.y16, pov.allfams=mult.im.pov.allfams.y16 )

preds7 <- predict.ppm(ppm.reduced2.hybrid.mult, window=o1.15, covariates = im.data3, ngrid=500, type="trend")

# make sure ppp1.15 includes the points for the year 2016
ppm.reduced2.hybrid.mult.offset <-  ppm( ppp1.15 ~ offset(log(prior.dens)) + pop + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( prior.dens=im.burg.avg.13to15, pop=mult.im.pop, a1=mult.im.a1, a2=mult.im.a2, a3=mult.im.a3, a4=mult.im.a4, a6=mult.im.a6, med.income=mult.im.med.income, pov.allfams=mult.im.pov.allfams, edu.25noHS.percent=mult.im.edu.25noHS.percent), subset = o1.15)

preds8 <- predict.ppm(ppm.reduced2.hybrid.mult.offset, window=o1.15, covariates = im.data3, ngrid=500, type="trend")
preds8.1 <- predict.ppm(ppm.reduced2.hybrid.mult.offset, window=o1.15, covariates = im.data3, ngrid=500, type="trend", interval="confidence" )


# aka im.burg.dens17.2
plot(im.burg.dens17.avgbw - preds8)
plot((im.burg.dens17.avgbw - preds8)^2)
sum((im.burg.dens17.avgbw - preds8)^2)
#6,305,474 ... much worse


# tried to rerun it while normalizing prior.density
im.burg.avg.13to15.norm <- im.burg.avg.13to15/sum(im.burg.avg.13to15)
im.burg.avg.14to16.norm <- im.burg.avg.14to16/sum(im.burg.avg.14to16)
ppm.reduced2.hybrid.mult.offset.norm <-  ppm( ppp1.15 ~ offset(log(prior.dens)) + pop + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( prior.dens=im.burg.avg.13to15.norm, pop=mult.im.pop, a1=mult.im.a1, a2=mult.im.a2, a3=mult.im.a3, a4=mult.im.a4, a6=mult.im.a6, med.income=mult.im.med.income, pov.allfams=mult.im.pov.allfams, edu.25noHS.percent=mult.im.edu.25noHS.percent), subset = o1.15)
im.data3.1 <- list(prior.dens=im.burg.avg.14to16.norm,  a1=mult.im.a1.y16, a2=mult.im.a2.y16, a3=mult.im.a3.y16, a4=mult.im.a4.y16, a6=mult.im.a6.y16, edu.25noHS.percent=mult.im.edu.25noHS.percent.y16, med.income=mult.im.med.income.y16, pop=mult.im.pop.y16, pov.allfams=mult.im.pov.allfams.y16 )
preds9 <- predict.ppm(ppm.reduced2.hybrid.mult.offset.norm, window=o1.15, covariates = im.data3, ngrid=500, type="trend")

# multiplied hybrid models are much worse, sticking with original offset models



ppm16.reduced2.offset.densavg.ho <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15, method="ho")

# trying to recreate estimates
#pg 308
im.burg.avg.13to15 * exp( -1.106734e+00  +    -9.669266e-06*im.pop.d   +    3.900653e+00*im.a1      -1.227218e+00*im.a2    +    8.603416e-01*im.a3    +   6.081205e+00*im.a4    +   4.063330e+00*im.a6      -6.605710e-06*im.med.income  +  -1.702608e+00*im.pov.allfams  +    -1.481133e+00*im.edu.25noHS.percent )

# the idea is that the sum of the intensity function across region B (prior year density * weighted covariates) should equal to the expected value of counts for the following year? 4,348 points
# problem is the values dont line up... is there a normalizing constant I need to include? 
# essentially, by using kernel density estimates I am using a fuzzier map than standard quadrature method
sum(im.burg.avg.13to15 * exp( -1.106734e+00  +    -9.669266e-06*im.pop.d   +    3.900653e+00*im.a1      -1.227218e+00*im.a2    +    8.603416e-01*im.a3    +   6.081205e+00*im.a4    +   4.063330e+00*im.a6      -6.605710e-06*im.med.income  +  -1.702608e+00*im.pov.allfams  +    -1.481133e+00*im.edu.25noHS.percent ))


#residual intensity difference between prior year and next year
plot(im.burg.avg.13to15 * exp( -1.106734e+00  +    -9.669266e-06*im.pop.d   +    3.900653e+00*im.a1      -1.227218e+00*im.a2    +    8.603416e-01*im.a3    +   6.081205e+00*im.a4    +   4.063330e+00*im.a6      -6.605710e-06*im.med.income  +  -1.702608e+00*im.pov.allfams  +    -1.481133e+00*im.edu.25noHS.percent ) - im.burg.avg.13to15 )

setwd("D:/R/R Data")
kppm16.reduced2.offset.densavg.thomas <- kppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), clusters = "Thomas", correction="border", subset = o1.15)
setwd("D:/Dropbox/Thesis Data/Thesis Code Redo Data 2")

setwd("D:/R/R Data")
kppm16.reduced2.offset.densavg.lgcp <- kppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), clusters = "LGCP", correction="border", subset = o1.15)
setwd("D:/Dropbox/Thesis Data/Thesis Code Redo Data 2")

# models failed to compile


ppm16.reduced2.offset.densavg.ho <- ppm( ppp16 ~ offset(log(burg.dens.prior)) + pop.d + a1 + a2 + a3 + a4 + a6 + med.income + pov.allfams + edu.25noHS.percent , covariates = list( burg.dens.prior=im.burg.avg.13to15, pop.d=im.pop.d, a1=im.a1, a2=im.a2, a3=im.a3, a4=im.a4, a6=im.a6, med.income=im.med.income, pov.allfams=im.pov.allfams, edu.25noHS.percent=im.edu.25noHS.percent), subset = o1.15, method="ho")
# File compiled but is too large to work with (memory overflow)


hist(fitted.ppm(ppm16.reduced2.offset.densavg))
plot.msr(ppm16.reduced2.offset.densavg)
integral.msr(ppm16.reduced2.offset.densavg)
plot.imlist()
plot.bermantest()
plot.studpermutest()

quad.pix.17 <- pixellate.ppp(ppp17, dimyx=c(500,500), W=o1.15, padzero=TRUE, DivideByPixelArea = FALSE)
quad.pix.17.big <- pixellate.ppp(ppp17, dimyx=c(125,125), W=o1.15, padzero=TRUE, DivideByPixelArea = FALSE)


while(i <= 500^2){
  pt1 <- poisson.test(x=quad.vals[i], r=rate.vals[i], conf.level=0.95, alternative="two.sided")
}


# This represents the prior average kernel intensity estimate...
plot(im.burg.dens17.2) # uses average bandwidth from prior years

# This represents the predicted intensity estimate given new data
plot(preds5$estimate)

# ...but then why are the values an order of magnitude
# different from the pixel counts??? 



plot(quad.pix.17)
plot(quad.pix.17.big)

# QUADRATURE SCHEME FOR MY MODEL
# read quad.object() help for more info

plot(ppm16.reduced2.offset.densavg$Q)
plot.quad(ppm16.reduced2.offset.densavg$Q)

#include this in quadrat count
plot.quadratcount( quadratcount(ppp16, 10))

qrcount1 <- quadratcount(ppp16, 128)

# Below are the paramater weights for the 140x140 pixel list
# convert to a matrix and plot the image!
qscheme.pix <- im( matrix(ppm16.reduced2.offset.densavg$Q$param$weight$areas, ncol=140, nrow=140))

# Anova test
anova.ppm(ppm16.baseline, ppm16.reduced2.offset.densavg, test="LR")

# tests for colinearity
round(vcov.ppm(ppm16.reduced2.offset.densavg, what="corr"), 2 )

# effect of individual covariates
#plot(effectfun(ppm16.reduced2.offset.densavg, covname = "pop.d", "burg.dens.prior", "a1", "a2", "a3", "a4", "a6", "med.income", "pov.allfams", "edu.25noHS.percent" ), se.fit=TRUE ))

kres.baseline <- Kres(ppm16.baseline, correction="border")
kres.reduced  <- Kres(ppm16.reduced2.offset.densavg)

leverage.baseline <- leverage(ppm16.baseline)
leverage.reduced  <- leverage(ppm16.reduced2.offset.densavg)

influence.baseline <- influence(ppm16.baseline)
influence.reduced  <- influence(ppm16.reduced2.offset.densavg)

plot(leverage.reduced )

# THINGS TO DO NEXT
# Find out why kres plot is not functioning
# See:  https://github.com/spatstat/spatstat/blob/master/R/Kinhom.R

