# Shapefile redo


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




#  IRS tax info
# 
# 1 = $1 under $25,000
# 2 = $25,000 under $50,000
# 3 = $50,000 under $75,000
# 4 = $75,000 under $100,000
# 5 = $100,000 under $200,000
# 6 = $200,000 or more


## START Code for adding additional 2013 year of sfd (Single-Family-Dwelling) burglary locations

burgs13 <- read.csv(file="D:/Downloads/Burglary Data 2013.csv", header=TRUE)

burgs13$Location <- as.character(burgs13$Location)


gsub1 <- gsub( "\\(" , "", burgs13$Location)
gsub2 <- gsub( "\\)", "", gsub1 )


strspt1 <- strsplit( gsub2, ",")

strspt1 <- unlist(strspt1)

Lat <- strspt1[seq(from = 1 , to = length(strspt1)-1, by = 2)]
Long <- strspt1[seq(from = 2 , to = length(strspt1), by = 2)]

burgs13$Lat <- as.numeric(Lat)
burgs13$Long <- as.numeric(Long)

burgs13$Row.num <- rep(NA, dim(burgs13)[1])
Row.num <- 1:dim(burgs13)[1]
burgs13 <- cbind(Row.num, burgs13[,1:28] ) 

rm(gsub1); rm(gsub2); rm(strspt1); rm(Row.num)


save( burgs13, file="burglaries1.2013.RData")

## END CODE for 2013 year of sfd burglary locations


#setwd("D:/R/R Data/Zip code prj files 1 DO NOT ALTER - Copy") original copy 
zipexcel <- read.csv("D:/R/R Data/Zip code prj files 1 DO NOT ALTER - Copy/CAMS_ZIPCODE_STREET_SPECIFIC.csv")
View(zipexcel)

table(burgs$Zip)

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
# if they don't there were some events that were lost in the loop 



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

# divide burglary count by shape area of the polygon (to preserve the average)



# Ideally a better way would be to use the over command to just find out exactly how many occur within zipcode regions



# Ultimately you tried to use image covariates for various regions (population+financials) to predict burglary counts (i.e rate of points per unit space/time) for the corresponding regions
# The problem is your image covariates are very crude, and interpolate over the entire area by averaging
# alternatively you can just try a non-parametric method... kernel density (KDE) + financial info


#find unique zips, make data table of counts, merge data tables with plyr.


zipexcel$avg.burg.counts <- round( (zipexcel$Shape_area.N.19.11/ zipexcel$zip.tot.area)*zipexcel$burg.counts, digits=2)

# old tabulation code folded  alt+L

# replaces left side (burglary counts for zip code entire region) with right side ( [sub region area/total region area]*burg counts entire zip code region ); 31, 123, 232 with values on left side
# zipexcel$avg.burg.counts[c(6,336:345,356)]<- round(zipexcel$Shape_area.N.19.11[c(6,336:345,356)]/sum(zipexcel$Shape_area.N.19.11[c(6,336:345,356)])*31, digits=2)
# zipexcel$avg.burg.counts[c(19,334)]<- round(zipexcel$Shape_area.N.19.11[c(19,334)]/sum(zipexcel$Shape_area.N.19.11[c(19,334)])*127, digits=2)
# zipexcel$avg.burg.counts[c(20,314)]<- round(zipexcel$Shape_area.N.19.11[c(20,314)]/sum(zipexcel$Shape_area.N.19.11[c(20,314)])*232, digits=2)
# zipexcel$avg.burg.counts[c(30,315)]<- round(zipexcel$Shape_area.N.19.11[c(30,315)]/sum(zipexcel$Shape_area.N.19.11[c(30,315)])*112, digits=2)
# zipexcel$avg.burg.counts[c(55,328,329)]<- round(zipexcel$Shape_area.N.19.11[c(55,328,329)]/sum(zipexcel$Shape_area.N.19.11[c(55,328,329)])*30, digits=2)
# zipexcel$avg.burg.counts[c(218,325)]<- round(zipexcel$Shape_area.N.19.11[c(218,325)]/sum(zipexcel$Shape_area.N.19.11[c(218,325)])*1, digits=2)
# zipexcel$avg.burg.counts[c(222,332)]<- round(zipexcel$Shape_area.N.19.11[c(222,332)]/sum(zipexcel$Shape_area.N.19.11[c(222,332)])*4, digits=2)
# zipexcel$avg.burg.counts[c(63,331)]<- round(zipexcel$Shape_area.N.19.11[c(63,331)]/sum(zipexcel$Shape_area.N.19.11[c(63,331)])*1, digits=2)
# zipexcel$avg.burg.counts[c(219,322)]<- round(zipexcel$Shape_area.N.19.11[c(219,322)]/sum(zipexcel$Shape_area.N.19.11[c(219,322)])*65, digits=2)
# zipexcel$avg.burg.counts[c(92,323)]<- round(zipexcel$Shape_area.N.19.11[c(92,323)]/sum(zipexcel$Shape_area.N.19.11[c(92,323)])*244, digits=2)


# zipexcel$avg.burg.counts[c(200,346)]<- round(zipexcel$Shape_area.N.19.11[c(200,346)]/sum(zipexcel$Shape_area.N.19.11[c(200,346)])*98, digits=2)
# zipexcel$avg.burg.counts[c(6,336:345,356)]<- round(zipexcel$Shape_area.N.19.11[c(6,336:345,356)]/sum(zipexcel$Shape_area.N.19.11[c(6,336:345,356)])*2, digits=2)
# zipexcel$avg.burg.counts[c(19,334)]<- round(zipexcel$Shape_area.N.19.11[c(19,334)]/sum(zipexcel$Shape_area.N.19.11[c(19,334)])*72, digits=2)
# zipexcel$avg.burg.counts[c(20,314)]<- round(zipexcel$Shape_area.N.19.11[c(20,314)]/sum(zipexcel$Shape_area.N.19.11[c(20,314)])*178, digits=2)
# zipexcel$avg.burg.counts[c(30,315)]<- round(zipexcel$Shape_area.N.19.11[c(30,315)]/sum(zipexcel$Shape_area.N.19.11[c(30,315)])*74, digits=2)
# zipexcel$avg.burg.counts[c(55,328,329)]<- round(zipexcel$Shape_area.N.19.11[c(55,328,329)]/sum(zipexcel$Shape_area.N.19.11[c(55,328,329)])*24, digits=2)
# zipexcel$avg.burg.counts[c(222,332)]<- round(zipexcel$Shape_area.N.19.11[c(222,332)]/sum(zipexcel$Shape_area.N.19.11[c(222,332)])*1, digits=2)
# zipexcel$avg.burg.counts[c(219,322)]<- round(zipexcel$Shape_area.N.19.11[c(219,322)]/sum(zipexcel$Shape_area.N.19.11[c(219,322)])*46, digits=2)
# zipexcel$avg.burg.counts[c(92,323)]<- round(zipexcel$Shape_area.N.19.11[c(92,323)]/sum(zipexcel$Shape_area.N.19.11[c(92,323)])*182, digits=2)


# forgot I need to segment my data by year before doing any of this, will create 4 separate burglary
# files, one for each year as well as one master file that has all years combined. Combining years
# will change these tabulations.


# zipexcel$avg.burg.counts[c(200,346)]<- round(zipexcel$Shape_area.N.19.11[c(200,346)]/sum(zipexcel$Shape_area.N.19.11[c(200,346)])*131, digits=2)
# zipexcel$avg.burg.counts[c(6,336:345,356)]<- round(zipexcel$Shape_area.N.19.11[c(6,336:345,356)]/sum(zipexcel$Shape_area.N.19.11[c(6,336:345,356)])*3, digits=2)
# zipexcel$avg.burg.counts[c(19,334)]<- round(zipexcel$Shape_area.N.19.11[c(19,334)]/sum(zipexcel$Shape_area.N.19.11[c(19,334)])*176, digits=2)
# zipexcel$avg.burg.counts[c(20,314)]<- round(zipexcel$Shape_area.N.19.11[c(20,314)]/sum(zipexcel$Shape_area.N.19.11[c(20,314)])*178, digits=2)
# zipexcel$avg.burg.counts[c(30,315)]<- round(zipexcel$Shape_area.N.19.11[c(30,315)]/sum(zipexcel$Shape_area.N.19.11[c(30,315)])*74, digits=2)
# zipexcel$avg.burg.counts[c(55,328,329)]<- round(zipexcel$Shape_area.N.19.11[c(55,328,329)]/sum(zipexcel$Shape_area.N.19.11[c(55,328,329)])*24, digits=2)
# zipexcel$avg.burg.counts[c(222,332)]<- round(zipexcel$Shape_area.N.19.11[c(222,332)]/sum(zipexcel$Shape_area.N.19.11[c(222,332)])*1, digits=2)
# zipexcel$avg.burg.counts[c(219,322)]<- round(zipexcel$Shape_area.N.19.11[c(219,322)]/sum(zipexcel$Shape_area.N.19.11[c(219,322)])*46, digits=2)
# zipexcel$avg.burg.counts[c(92,323)]<- round(zipexcel$Shape_area.N.19.11[c(92,323)]/sum(zipexcel$Shape_area.N.19.11[c(92,323)])*182, digits=2)


# going to take a few minutes to think about how im going to approach this problem.

# 1) cut down regions to a smaller subsection of zip codes that are 100% inside the region
# 2) label/cut data into subsets of 3 years
# 4) Inputs=median income/tax data/kernel density plot of prior year?, output=counts of burgs for each zip code region.

# collect 2 years of tax data/population$median income (zcta5)/burg counts with burg counts lagged by 1 year
# zcta5 tigershapefiles


# **** Code used to be here that manually imput the population data values for 2015 ***


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

#update auxillary data in datatable subfile (DBF) of shapefile
# setwd("C:/Users/Ryan/Dropbox/Thesis Data/Thesis Code Redo Data/Zip code prj files 2")
# write.csv(zipexcel, file = "CAMS_ZIPCODE_STREET_SPECIFIC.csv", row.names=FALSE) # note after this convert back to dbf format!!!
# setwd("C:/Users/Ryan/Dropbox/Thesis Data/Thesis Code Redo Data")

# load("burglaries6.RData")

burgs$Time.Occurred <- as.POSIXct(burgs$Time.Occurred)
burgs$Time.Reported <- as.POSIXct(burgs$Time.Reported)


# This code needs to be fixed
# restrict zipexcel dataframe to non-duplicated rows
# zipexcel[!duplicated(zipexcel$Zip_Num.N.10.0), c(8,13)]

burgs$Zip <- as.numeric(burgs$Zip)

burgs <- left_join( x = burgs , y = zipexcel[!duplicated(zipexcel$Zip_Num.N.10.0), c(8,13)] , by = c("Zip" = "Zip_Num.N.10.0") )

# left join adds rows because multiple matches (duplicate zipcode names) in zipexcel

# NOTE DOUBLE CHECK PREVIOUS LEFTJOIN TO ENSURE NO DUPLICATE ROWS WERE ADDED!!!!
# STATUS: CHECKED

# save(burgs, file="burglaries7.RData") # burglary data before ordinal cutting


# next steps import pop/demographic data/tax data
# start modeling
# cross validate

# In the meantime you can writeup your methods and start including descriptive stats
# Make a folder of graphics you will use to describe data (exploratory analysis)
# Find the Latex template for your paper
# Find sources that show poisson regression using demographics/economic info might be an interesting predictor


# data sources for join functions
 edudat <- read.csv(file.choose(), header=TRUE)
 povdat <- read.csv(file.choose(), header=TRUE)
 housedat <- read.csv(file.choose(), header=TRUE)

# merge these tables with zipexcel

zipexcel2 <- left_join( zipexcel, edudat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, povdat, by = c("Zip_Num.N.10.0" = "zipnames"))
zipexcel2 <- left_join( zipexcel2, housedat, by = c("Zip_Num.N.10.0" = "zipnames"))

zipexcel2 <- zipexcel2[,-c(25:27,32:34,38)]

zipexcel <- zipexcel2

zipexcel15 <- zipexcel

zipexcel15 <- zipexcel15[,-24]
save(zipexcel15, file = "Zipexcel15_df")


write.csv(zipexcel15, file="D:/Dropbox/Thesis Data/Thesis Code Redo Data 2/Zip code prj files 2/CAMS_ZIPCODE_STREET_SPECIFIC.csv", row.names=FALSE)
# update dbf file!!!

# repeat code for 2016 covariates

burgs <- burgs17

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

popdat <- read.csv(file.choose(), header=TRUE)

zipexcel2 <- left_join( zipexcel2, popdat, by = c("Zip_Num.N.10.0" = "zipnames"))


zipexcel16 <- zipexcel2
save(zipexcel16, file = "Zipexcel16_df")

write.csv(zipexcel16, file="D:/Dropbox/Thesis Data/Thesis Code Redo Data 2/Zip code prj files 3/CAMS_ZIPCODE_STREET_SPECIFIC.csv", row.names=FALSE)

# update dbf file! Should be lots of NAs for regions with no data
# when plotting regions make sure to subset zipcodes matching zl list


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

# oh... I was expecting them to be different but forgot I already removed zip codes that do not appear at least once in either 2015-2016 (covarate data)
# now the question is... will 84 observations be enough... I guess its fine for a simple test.
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


# Find way to only read in the 84 zip code regions

zipbounds1 <- readOGR("D:/Dropbox/Thesis Data/Thesis Code Redo Data 2/Zip code prj files 2", "CAMS_ZIPCODE_STREET_SPECIFIC")

 

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



# use this code or nah?
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

burgs.all$Long <- c(burgs13$Long, burgs$Long)
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


# PRIMARY MODEL USED!!! THREE YEAR AVERAGE OF PRIOR DENSITY WITH OFFSET AND ALL TERMS
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



########### START BIG LIST OF PLOTS ############

# List of plots


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

setwd("D:/Thesis Paper")
save( sims.ppm16.1, file="sims.ppm16.1.RData")
save( sims.ppm16.base.1, file="sims.ppm16.base.1.RData")



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