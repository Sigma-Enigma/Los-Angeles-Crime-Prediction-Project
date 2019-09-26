
# This file consists of mainly data manipulation and table merging


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



#### TABLE DATA ####
#  IRS tax info
# 
# 1 = $1 under $25,000
# 2 = $25,000 under $50,000
# 3 = $50,000 under $75,000
# 4 = $75,000 under $100,000
# 5 = $100,000 under $200,000
# 6 = $200,000 or more



# load WD from end of crime data cleanup
# setwd("D:/R Demo WD")
load("LAPD_Crime_Data_Cleanup_Image.RData")



#############################################################
#######  BEGIN CODE TO MERGE 2014-2017 ZIPCODE TABLE DATA  #######

# Ideally, the code from here to END would be made into a function

#setwd("D:/R/R Data/Zip code prj files 1 DO NOT ALTER - Copy") original copy 
zipexcel <- read.csv("D:/R/R Data/Zip code prj files 1 DO NOT ALTER - Copy/CAMS_ZIPCODE_STREET_SPECIFIC.csv")

# to see tabular zipcode region data
# View(zipexcel) 

table(burgs$Zip)   
plot(as.factor(burgs$Zip))

# rerun this count and build a table for each year burgs16, burgs17
# covariates should be of the year before


# should these burgs be from 2016 year with 2015 covariates??
zipcounts <- data.frame(Zip = as.factor(burgs$Zip))
ziptable <- zipcounts %>% 
  group_by(Zip) %>%
  summarise(no_rows = length(Zip)) # burgs$Zip?


zipexcel$burg.counts <- rep(0, dim(zipexcel)[1])

sort(unique(zipexcel$Zip_Num.N.10.0)) # Set A
sort(unique(burgs$Zip)) # Set B

# Set B should be entirely contained within set A, however there may be a few values not found in A

# checks if B is found in A
sort(unique(burgs$Zip)) %in% sort(unique(zipexcel$Zip_Num.N.10.0))


i <- 1
j <- 1

# This assigns each shapefile object (rows in zipexcel) the appropriate number of burglaries that occured across the entire zipcode region
# The next step we average the number of burglaries over the total area of each zip code region, so we dont double count depending on number of times a zip code region is divided

# length(ziptable$no_rows) == 123 in our case

while(i <= dim(zipexcel)[1]) {
  if( (zipexcel$Zip_Num.N.10.0[i] == ziptable$Zip[j]) & ( j <= length(ziptable$no_rows)) ) {
    zipexcel$burg.counts[i] <- ziptable$no_rows[j]
    i <- i + 1
    j <- 1
  } else if ( (zipexcel$Zip_Num.N.10.0[i] != ziptable$Zip[j]) & ( j == length(ziptable$no_rows)) ) {
    i <- i + 1
    j <- 1
  } else if ( (zipexcel$Zip_Num.N.10.0[i] == ziptable$Zip[j]) & ( j == length(ziptable$no_rows)) ) {
    i <- i + 1
    j <- 1
  } else { j = j+1 }
}

# Using only rows with unique zip codes, check if they sum to dim(burgs)[1]
zipexcel$burg.counts[!duplicated(zipexcel$Zip_Num.N.10.0)]

dfcheck <- zipexcel[!duplicated(zipexcel$Zip_Num.N.10.0), c("Zip_Num.N.10.0", "burg.counts")] 

table(burgs$Zip)

sum(zipexcel$burg.counts[!duplicated(zipexcel$Zip_Num.N.10.0)])
dim(burgs)[1]
# if they don't there were some events that were lost in the loop ... yup it checks out



zipexcel$avg.burg.counts <- zipexcel$burg.counts

#
zipregs1 <- as.data.frame(table(zipexcel$Zip_Num.N.10.0))
zipregs1$Var1 <- as.numeric(as.character(zipregs1$Var1))
zipregs1$zip.tot.area <- rep(0, dim(zipregs1)[1])

i <- 1
while(i <= dim(zipregs1)[1]){
  zipregs1$zip.tot.area[i] <- sum( zipexcel$Shape_area.N.19.11[which( zipexcel$Zip_Num.N.10.0 == zipregs1$Var1[i])] )
  i <- i+1
}

zipexcel <- left_join(zipexcel, zipregs1, by= c("Zip_Num.N.10.0" = "Var1") )

zipexcel$num.zip.regions <- zipexcel$Freq
zipexcel$Freq <- NULL

# divide burglary count by shape area of the polygon (to preserve the average)



# Ideally a better way would be to use the over command to just find out exactly how many occur within zipcode regions



# Ultimately you tried to use image covariates for various regions (population+financials) to predict burglary counts (i.e rate of points per unit space/time) for the corresponding regions
# The problem is your image covariates are very crude, and interpolate over the entire area by averaging
# alternatively you can just try a non-parametric method... kernel density (KDE) + financial info


#find unique zips, make data table of counts, merge data tables with plyr.


zipexcel$avg.burg.counts <- round( (zipexcel$Shape_area.N.19.11/ zipexcel$zip.tot.area)*zipexcel$burg.counts, digits=2)



# going to take a few minutes to think about how im going to approach this problem.

# 1) cut down regions to a smaller subsection of zip codes that are 100% inside the region
# 2) label/cut data into subsets of 3 years
# 4) Inputs=median income/tax data/kernel density plot of prior year?, output=counts of burgs for each zip code region.

# collect 2 years of tax data/population$median income (zcta5)/burg counts with burg counts lagged by 1 year
# zcta5 tigershapefiles


# adding irs_finance stats to shapefile of zip code regions

zipcounts <- data.frame(Zip = as.factor(burgs$Zip))
ziptable <- zipcounts %>% 
  group_by(Zip) %>%
  summarise(no_rows = length(Zip))

load("irs_finance.RData")

irs_finance_clean <- irs_finance %>%
  group_by(zipcode) %>%
  summarise(Total.Exemptions = sum(N2),
            Total.Returns = sum(N02650),
            Percent.agi1 = first(N02650)/sum(N02650), 
            Percent.agi2 = nth(N02650, 2)/sum(N02650), 
            Percent.agi3 = nth(N02650, 3)/sum(N02650), 
            Percent.agi4 = nth(N02650, 4)/sum(N02650), 
            Percent.agi5 = nth(N02650, 5)/sum(N02650), 
            Percent.agi6 = nth(N02650, 6)/sum(N02650), 
            Total.Income = sum(A02650)) 
View(irs_finance_clean)


# The above includes all CA zip codes (including ones outside of my shapefile boundaries)
# Next step is to append this set with the zipexcel table by matching zip code regions
# Done here
zipexcel2 <- left_join( zipexcel, irs_finance_clean, by = c("Zip_Num.N.10.0" = "zipcode"))


zipexcel <- zipexcel2

save(zipexcel, file = "Zipexcel_df")

# update auxillary data in datatable subfile (DBF) of shapefile
# setwd("C:/Users/Ryan/Dropbox/Thesis Data/Thesis Code Redo Data/Zip code prj files 2")
# write.csv(zipexcel, file = "CAMS_ZIPCODE_STREET_SPECIFIC.csv", row.names=FALSE) # note after this convert back to dbf format!!!
# setwd("C:/Users/Ryan/Dropbox/Thesis Data/Thesis Code Redo Data")

# load("burglaries7.2016.RData")



burgs16$Time.Occurred <- as.POSIXct(burgs16$Time.Occurred)
burgs16$Time.Reported <- as.POSIXct(burgs16$Time.Reported)



burgs16$Zip <- as.numeric(burgs16$Zip)

burgs16 <- left_join( x = burgs16 , y = zipexcel[!duplicated(zipexcel$Zip_Num.N.10.0), c(8,13)] , by = c("Zip" = "Zip_Num.N.10.0") )

# left join adds rows because multiple matches (duplicate zipcode names) in zipexcel

# NOTE: DOUBLE CHECK PREVIOUS LEFTJOIN TO ENSURE NO DUPLICATE ROWS WERE ADDED!!!!
# STATUS: CHECKED



# next steps import pop/demographic data/tax data
# start modeling
# cross validate

# In the meantime you can writeup your methods and start including descriptive stats
# Make a folder of graphics you will use to describe data (exploratory analysis)
# Find the Latex template for your paper
# Find sources that show poisson regression using demographics/economic info might be an interesting predictor





# THESE THREE TABLES CONTAIN ZIP-CODE SPECIFIC DEMOGRAPHIC DATA (predictor)
# ON EDUCATIONAL ATTAINTMENT, POVERTY AND HOUSING FOR 2015 and 2016 years
# consider hosting this data somewhere later

load("edudat.RData")
load("povdat.RData")
load("housedat.RData")


# merge these tables with zipexcel to augment shapefile table data

zipexcel2 <- left_join( zipexcel, edudat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, povdat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, housedat, by = c("Zip_Num.N.10.0" = "zipnames"))

zipexcel2 <- zipexcel2[,-c(25:27,32:34,38)]

zipexcel <- zipexcel2

zipexcel15 <- zipexcel

zipexcel15 <- zipexcel15[,-24]
save(zipexcel15, file = "Zipexcel15_df")


write.csv(zipexcel15, file="D:/R Demo WD/CAMS_ZIPCODE_STREET_SPECIFIC_2015.csv", row.names=FALSE)
# update dbf file!!!


#######  END CODE TO MERGE 2015 ZIPCODE TABLE DATA  #######
###########################################################
# repeat this merge for 2016 data




#############################################################
#######  BEGIN CODE TO MERGE 2016 ZIPCODE TABLE DATA  #######

burgs <- burgs17 ## ?????????????????? WHICH BURGS IS THIS ????????????????????

#####   ????????? WAS THIS RUN MULTIPLE TIMES ITERATING THE YEAR NUMBER ??????????

# zipexcel <- read.csv("D:/R/R Data/Zip code prj files 1 DO NOT ALTER - Copy/CAMS_ZIPCODE_STREET_SPECIFIC.csv")

table(burgs$Zip)
length(table(burgs$Zip))

# rerun this count and build a table for each year burgs16, burgs17
# covariates should be of the year before
zipcounts <- data.frame(Zip = as.factor(burgs$Zip))
ziptable <- zipcounts %>% 
  group_by(Zip) %>%
  summarise(no_rows = length(Zip)) # burgs$Zip?


zipexcel$burg.counts <- rep(0, dim(zipexcel)[1])

sort(unique(zipexcel$Zip_Num.N.10.0)) #Set A
sort(unique(burgs$Zip)) #Set B

# Set B should be entirely contained within set A, howver there may be a few values not found in A

# checks if B is found in A
sort(unique(burgs$Zip)) %in% sort(unique(zipexcel$Zip_Num.N.10.0))

i <- 1
j <- 1

while(i <= dim(zipexcel)[1]) {
  if( (zipexcel$Zip_Num.N.10.0[i] == ziptable$Zip[j]) & ( j <= length(ziptable$no_rows)) ) {
    zipexcel$burg.counts[i] <- ziptable$no_rows[j]
    i <- i + 1
    j <- 1
  } else if ( (zipexcel$Zip_Num.N.10.0[i] != ziptable$Zip[j]) & ( j == length(ziptable$no_rows)) ) {
    i <- i + 1
    j <- 1
  } else if ( (zipexcel$Zip_Num.N.10.0[i] == ziptable$Zip[j]) & ( j == length(ziptable$no_rows)) ) {
    i <- i + 1
    j <- 1
  } else { j = j+1 }
}

# Using only rows with unique zip codes, check if they sum to dim(burgs)[1]
zipexcel$burg.counts[!duplicated(zipexcel$Zip_Num.N.10.0)]

dfcheck <- zipexcel[!duplicated(zipexcel$Zip_Num.N.10.0), c("Zip_Num.N.10.0", "burg.counts")] 

table(burgs$Zip)

sum(zipexcel$burg.counts[!duplicated(zipexcel$Zip_Num.N.10.0)])

zipexcel$avg.burg.counts <- zipexcel$burg.counts

zipregs1 <- as.data.frame(table(zipexcel$Zip_Num.N.10.0))
zipregs1$Var1 <- as.numeric(as.character(zipregs1$Var1))
zipregs1$zip.tot.area <- rep(0, dim(zipregs1)[1])

i <- 1
while(i <= dim(zipregs1)[1]){
  zipregs1$zip.tot.area[i] <- sum( zipexcel$Shape_area.N.19.11[which( zipexcel$Zip_Num.N.10.0 == zipregs1$Var1[i])] )
  i <- i+1
}

zipexcel <- left_join(zipexcel, zipregs1, by= c("Zip_Num.N.10.0" = "Var1") )

zipexcel$num.zip.regions <- zipexcel$Freq
zipexcel$Freq <- NULL

zipexcel$avg.burg.counts <- round( (zipexcel$Shape_area.N.19.11/ zipexcel$zip.tot.area)*zipexcel$burg.counts, digits=2)

zipcounts <- data.frame(Zip = as.factor(burgs17$Zip))
ziptable <- zipcounts %>% 
  group_by(Zip) %>%
  summarise(no_rows = length(Zip))

load("irs_finance.RData")

irs_finance_clean <- irs_finance %>%
  group_by(zipcode) %>%
  summarise(Total.Exemptions = sum(N2),
            Total.Returns = sum(N02650),
            Percent.agi1 = first(N02650)/sum(N02650), 
            Percent.agi2 = nth(N02650, 2)/sum(N02650), 
            Percent.agi3 = nth(N02650, 3)/sum(N02650), 
            Percent.agi4 = nth(N02650, 4)/sum(N02650), 
            Percent.agi5 = nth(N02650, 5)/sum(N02650), 
            Percent.agi6 = nth(N02650, 6)/sum(N02650), 
            Total.Income = sum(A02650)) 
View(irs_finance_clean)


# The above includes all CA zip codes (including ones outside of my shapefile boundaries)
# Next step is to append this set with the zipexcel table by matching zip code regions
# Done here
zipexcel2 <- left_join( zipexcel, irs_finance_clean, by = c("Zip_Num.N.10.0" = "zipcode"))

zipexcel3 <- zipexcel2

zipexcel2 <- left_join( zipexcel2, edudat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, povdat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, housedat, by = c("Zip_Num.N.10.0" = "zipnames"))

names(zipexcel2)
zipexcel2 <- zipexcel2[,-c(28:31,35:37,39)]

popdat <- read.csv(file.choose(), header=TRUE) # ??????????? find old file and save directly to R demo WD

zipexcel2 <- left_join( zipexcel2, popdat, by = c("Zip_Num.N.10.0" = "zipnames"))


zipexcel16 <- zipexcel2
save(zipexcel16, file = "Zipexcel16_df")

write.csv(zipexcel16, file="D:/R Demo WD/CAMS_ZIPCODE_STREET_SPECIFIC_2016.csv", row.names=FALSE)

# update dbf file! Should be lots of NAs for regions with no data
# when plotting regions make sure to subset zipcodes matching zl list



#######  END CODE TO MERGE 2016 ZIPCODE TABLE DATA  #######
###########################################################


# next clean out NA's from 2015, and select the zip code regions you will include in analyses
# check to make sure there is good overlap of regions, exlude low count (undersampled) regions (after checking that they are outside LAPD jurisdiciton)

# remember dates are based on imput variables, output is the burglaries of the following year



z15sbs1 <- subset(zipexcel15, burg.counts >= 1)

z15sbs1u <- z15sbs1[!duplicated(z15sbs1$Zip_Num.N.10.0),]

z16sbs1 <- subset(zipexcel16, burg.counts >= 1)

z16sbs1u <- z16sbs1[!duplicated(z16sbs1$Zip_Num.N.10.0),]

length(union(unique(z15sbs1u$Zip_Num.N.10.0), unique(z16sbs1u$Zip_Num.N.10.0) ))

# Final list of zip code regions with at least 1 burglary in 2016-2017 
union(unique(z15sbs1u$Zip_Num.N.10.0), unique(z16sbs1u$Zip_Num.N.10.0) ) 

# We can argue that the distribution is skewed right
# So a Poisson distribution would be appropriate (Or Negative Binomial)
hist(z15sbs1u$burg.counts, breaks=50, main="Burglary Count Distribution")
plot(density(z15sbs1u$burg.counts))
table(z15sbs1u$burg.counts)

# going to take a moment to think of how to best model the count data
# might just do run of the mill poisson/negative binomial multivariate regression


# need to make sure they are all in the LA city boundary to finalize their inclusion

# 6/2/18 next thing to do: 
# Finish checking zip boundaries and get finalized list of areal regions
# plot the various covariates on the map (image command)
# consider doing a 3d plot with time as a dimension... if you have time


# finalized list if zip codes: 90021, 90077,  91330, 90045, 90089, 91331, 90003, 90024, 90094, 91335, 91601, 90004, 90095, 90731, 91602, 90005, 90026, 90732, 91604, 90005, 90027, 90211, 90732, 91343, 91605, 90006, 90028, 90056, 90212, 90744, 91344, 91606, 90007, 90029, 90057, 91345, 91607, 90031, 91040, 91352,  90010, 90032, 91356, 90011, 90033, 90012, 90034, 90062, 91303, 91367, 90013, 90035, 90272, 91401, 90014, 90036, 90064, 91306, 91402, 90015, 90037, 90065, 91307, 91403, 90016, 90038, 91405, 90017, 90039, 90067, 91316, 91406, 90018, 90041, 90068, 91324, 91411, 90019, 90042, 91325, 91423, 90020, 90071, 91436


unique(zipexcel15$Zip_Num.N.10.0)

zl <- c(90021, 90077,  91330, 90045, 90089, 91331, 90003, 90024, 90094, 91335, 91601, 90004, 90095, 90731, 91602, 90005, 90026, 90732, 91604, 90005, 90027, 90211, 90732, 91343, 91605, 90006, 90028, 90056, 90212, 90744, 91344, 91606, 90007, 90029, 90057, 91345, 91607, 90031, 91040, 91352,  90010, 90032, 91356, 90011, 90033, 90012, 90034, 90062, 91303, 91367, 90013, 90035, 90272, 91401, 90014, 90036, 90064, 91306, 91402, 90015, 90037, 90065, 91307, 91403, 90016, 90038, 91405, 90017, 90039, 90067, 91316, 91406, 90018, 90041, 90068, 91324, 91411, 90019, 90042, 91325, 91423, 90020, 90071, 91436)
sort(zl) %in% sort(unique(zipexcel15$Zip_Num.N.10.0))

ezl <- 0 # the extended zipcode list with more regions

v1 <-  unique(zipexcel15$Zip_Num.N.10.0)
v2 <-  sort(unique(zipexcel15$Zip_Num.N.10.0)) %in% sort(zl) 
v3 <-  sort(unique(zipexcel16$Zip_Num.N.10.0)) %in% sort(zl) 
v4 <- v2 == v3
View(as.data.frame(list(v1,v2,v3,v4)))

ze15 <- subset(zipexcel15, !duplicated(zipexcel15$Zip_Num.N.10.0) )
ze15 <- subset(ze15, ze15$Zip_Num.N.10.0 %in% zl)

ze16 <- subset(zipexcel16, !duplicated(zipexcel16$Zip_Num.N.10.0) )
ze16 <- subset(ze16, ze16$Zip_Num.N.10.0 %in% zl)

# begin poisson regressions, 2 ways, aggregate, or piecewise 



# Notes ################################################################################## NOTE: this code needs to be cleaned. It appears that point of the output was to generate a zipcode list (zl), however the final outputs (ze15,ze16) are never used again in the entire project. There is naming confusion with burgs (it may have been used as a temporary variable representing EITHER burgs for all years 2014-2017 OR burgs of individual years; this depends on if this code was recycled and used to overwrite itself).
