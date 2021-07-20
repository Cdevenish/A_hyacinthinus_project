
## Scripts for analysis in:

## Multi-decadal land-use impacts across the vast range of an iconic threatened species
## Diversity and Distributions
## Christian Devenish, Alexander C. Lees, Nigel J. Collar, Stuart J. Marsden


## Script s1 Merge and filter occurrence records

## Load occurrence points and filter

library(sf)
source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r") # open data frame in excel
source("code/minDistPoint.r") # thin by distance between points

# set your home directory:
# setwd("PATH/TO/github/repo/A_hyacinthinus_project")

# projection information:
# EPSG:5880
# SIRGAS 2000 / Brazil Polyconic
# proj4.defs("EPSG:5880","+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs");

sirgas <- 5880
wgs <- 4326


### Load occurrence points
allRecs <- read.csv("data/Table_S1b_Supp_Info.csv")
head(allRecs)
str(allRecs)

# remove wiki records without coords
allRecs <- subset(allRecs, !is.na(lat))

## checked in arcgis - remove uncertain records without good evidence... see below
#all.sf <- st_as_sf(allRecs, coords = c("lon", "lat"), crs = wgs, agr = "constant")
#st_write(all.sf, "gis/s_wgs20/allRecs.shp", delete_layer = TRUE)

# These records
# vert0020 # old, not in range
# ebird0329 # unlikely - not in range, no rich media
# ebird0421 # heavily birded area, only recorded in state, almost. Would be other records.
# ebird0803 # birdlist includes pantanal species. wrong location
# ebird0943 # unlikely, not in range, no rich media
# URN:CornellLabOfOrnithology:EBIRD:OBS920540252  in bolivia, and way out. mistake?
#	ebird0963

# wiki2407 # way out of range. escapes probably
# wiki2408 # # way out of range. escapes probably
# wiki2468 # # way out of range. escapes probably
# remove these records: escapes
# c("wiki2067", "wiki2068") # no coords anyway... 

recsOut <- c("vert0020", "ebird0329", "ebird0421", "ebird0803", "ebird0963",
             "ebird0943", "wiki2407", "wiki2408", "wiki2468", 
             "wiki2067", "wiki2068")
sum(recsOut %in% allRecs$dataID)

allRecs <- allRecs[!allRecs$dataID %in% recsOut,]


## Get distance to urban areas
## make sf object
all.wgs <- st_as_sf(allRecs, coords = c("lon", "lat"), crs = wgs)

## transform to sirgas
all.sirgas <- st_transform(all.wgs, sirgas)

## Bring in cities and urban areas (from https://www.ibge.gov.br/). Download and save to data folder
## (We don't have rights to upload and store these on github - but are available above). Otherwise send me an email.

# Urban points
cidade.ibge <- st_read("data/lml_cidade_p.shp")
cidade.ibge

# Urban areas
urb.area <- st_read("data/lml_area_densamente_edificada_a.shp")

# transform to my proj
urb.sirg <- st_transform(urb.area, sirgas)
cidade.sirg <- st_transform(cidade.ibge, sirgas)

# get distance to closest points - city points
distM <- st_distance(all.sirgas, cidade.sirg, which = "Euclidean")
dim(distM) # rows are occurence points # 4156 5570
distM[1:10, 1:10] # distances

# to urban areas (poly)
distUr <- st_distance(all.sirgas, urb.sirg, which = "Euclidean")
dim(distUr)

# so closest distance is row Mins 
minDist_pt <- apply(distM, 1, min)
hist(minDist_pt)
all.sirgas$minDist_urbPt <- minDist_pt

# get name of nearest city and add to data frame
cid.Ind <- apply(distM, 1, which.min)
all.sirgas$minCidade <- cidade.sirg$nome[cid.Ind]
all.sirgas$minDist_urbAr <- apply(distUr, 1, min)

#
head(all.sirgas)

# check out distances
table(cut(all.sirgas$minDist_urbPt, breaks = c(0,100,1000,5000,10000,50000,100000,200000)))

# check which datasets are closest to cities
boxplot(minDist_urbPt ~ dataSet, data = all.sirgas)
boxplot(minDist_urbAr ~ dataSet, data = all.sirgas)

hist(all.sirgas$minDist_urbAr[all.sirgas$dataSet == "wikiaves"])
summary(all.sirgas$minDist_urbAr[all.sirgas$dataSet == "wikiaves"])
table(all.sirgas$locality[all.sirgas$dataSet == "wikiaves"]) # these distances must be from Pocone and Aquidauna.... 
table(all.sirgas$dataSet, all.sirgas$minDist_urbPt < 1000)
table(all.sirgas$dataSet, all.sirgas$minDist_urbPt < 500)
table(all.sirgas$dataSet, all.sirgas$minDist_urbAr < 250)

### 2. OCcurrence record filtering ######

## 2a by YEAR. ####

# restrict occurrence points to > 1995 (allows 10 year window before 1st occurence.. ) and <= 2019 (last year of mapbiomas)
# and remove all points without date
table(all.sirgas$year)

sum(is.na(all.sirgas$year))
# 164
table(all.sirgas$year >= 1995)
# FALSE  TRUE 
# 69  1500 

sum(all.sirgas$year >= 1985 & all.sirgas$year < 1995, na.rm = T)  # 18 points between these dates
sum(all.sirgas$year > 2019, na.rm = T)
#79

all.sirgas <- subset(all.sirgas, year >= 1995 & year <= 2019 & !is.na(year))

## 2b Urban proximity ####

## Using filtered points for urban proximity.
## Filter all wihtin 500 m of urban city point OR within 500 m of urban area, or within urban area (NA)
sum(all.sirgas$minDist_urbPt <= 500 & all.sirgas$minDist_urbAr <= 500)
pts.pres <- subset(all.sirgas, minDist_urbPt > 500 & minDist_urbAr > 500)
table(pts.pres$dataSet)

## 2c. Geographic Filter - repeated locations ####

## remove 4 points outside Brazil (mapBiomas is only for Brazil)

unique(pts.pres$country)
sum(pts.pres$country %in% c("Bolivia", "Paraguay"))

pts.pres <- subset(pts.pres, !country %in% c("Bolivia", "Paraguay"))

pts.pres


## order by year 
pts.pres <- pts.pres[order(pts.pres$year), ]
head(pts.pres)

# thin points by distance - just to get set of points thinned by this distance. These are then buffered and intersected 
## with all points again, and points are chosen by distance and by date.
dist <- 5000
thin_modData_minD <- minDistPoint(pts.pres, dist = dist)
# 225 points remaining

table(thin_modData_minD$year)
head(thin_modData_minD)

# convert to sf and buffer with same dist 
thin.sf <- st_as_sf(thin_modData_minD, coords = c("x", "y"), crs = sirgas)
thin.5k.buff <- st_buffer(thin.sf, dist = dist)

# plot(st_geometry(thin.5k.buff), add = T)

# intersect buffers with all poitns, and choose earliest date within each buffer
buff.int.pts <- st_intersects(thin.5k.buff, pts.pres)
# str(buff.int.pts, max.level = 1)

# returns either the index of the single point or the index of the earliest date within buffer (index of pts.pres)
moreRecent <- sapply(buff.int.pts, function(x){
  
  if(length(x) == 1) return(x) else {
    
    ind <- which.min(data.frame(pts.pres)[x, "year"])
    x[ind]
    
  }
})

# pts.pres.sf[124, ]

# keep copy
all.pres <- pts.pres

pts.pres <- pts.pres[moreRecent, ]

## compare distribution of years
table(pts.pres$year)
# 1995 1997 1998 1999 2000 2001 2002 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 
# 2    1    2    4    3    6    4   10    5    5    3   17   13   13    5    9    9    6   12   15   30   25   26 
table(thin_modData_minD$year)
table(pts.pres$year)
# 1995 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 
# 2    1    2    3    3   12    5    3   11    5    6    3   21   17   28   17   35   43   66  125  115  242  288  332 

table(pts.pres$dataSet)
# eBird   icmbio      lit wikiaves 
# 137       13       54       21 

## save
save(allRecs, all.sirgas, pts.pres, all.pres, file = "data/allRecs.rdata")

