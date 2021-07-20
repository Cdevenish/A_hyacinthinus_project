
## Scripts for analysis in:

## Multi-decadal land-use impacts across the vast range of an iconic threatened species
## Diversity and Distributions
## Christian Devenish, Alexander C. Lees, Nigel J. Collar, Stuart J. Marsden


## Script s3 Create palm habitat suitability layer for predictor in main model

## 1 Download and merge palm data from BIEN, GBIF and data paper Balslev et al 2019
## 2. Filter occurrence points, prepare predictors 
## Make palm models with Maxent, ENMEval

## Make palm distribution models 
# setwd("PATH/TO/github/repo/A_hyacinthinus_project")

source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")# open data frame in excel
#install.packages("BIEN")
library(BIEN) # palms occ records download
library(sp)
library(sf)
library(dismo) # for GBIF download

#### 1 Download palm data ######
## first part downloads data from BIEN, GBIF and combines with data paper palm data and combines to sf object. 

# projection info
sirgas <- 5880
wgs <- 4326

# Load AOI - study area - convex hull around points and BL shapefile. see methods.
load("data/extents_aoi.rdata") # aoi, msk.aoi, ext.r, ext.bbox
rm(msk.aoi, ext.r)
aoi
aoi.wgs <- st_transform(aoi, wgs)

# get names of palms in diet/ nesting tree:
trees <- read.csv("data/Palms_Trees.csv", stringsAsFactors = F)
trees
str(trees)

spp <- unique(trees$Species)
gen <- sub("([[:alpha:]]*)\\ssp$", "\\1", spp[grep("\\ssp$", spp)])
spp <- spp[!grepl("\\ssp$", spp)]
gen
spp

# download species from BIEN
occ <- BIEN_occurrence_species(spp, new.world = T, all.taxonomy = T, native.status = T, 
                               observation.type = T, political.boundaries = T, collection.info = T)

head(occ)
# check species
spp %in% unique(occ$name_matched)
spp[!spp %in% unique(occ$name_matched)]

# export occurrence records and check
w.xls(occ)

# check coords
sum(is.na(occ$latitude))

colnames(occ)
occPalm.sf <- st_as_sf(occ, coords = c("longitude", "latitude"), crs = wgs)
occPalm.sf

occPalm.sir <- st_transform(occPalm.sf, sirgas)


plot(occPalm.sir[,"scrubbed_species_binomial"])

## Get data from GBIF

# get extent to limit download
plot(as(occPalm.sf[,1], "Spatial"))
# ext1 <- raster::drawExtent()
ext1 <- raster::extent(c(xmin = -80, xmax=-30, ymin = -31, ymax = 10))
# extent to poly
ext.poly <- st_sf(data.frame(geometry = st_as_sfc(st_bbox(ext1, crs = wgs))))
ext.poly

plot(st_geometry(occPalm.sf), axes = T)
plot(ext.poly, add = T)
plot(aoi.wgs, add = T)

gBIF <- list()

## download data for each species
for(i in seq_along(spp)){
  
  gBIF[[i]] <- dismo::gbif(genus = strsplit(spp[i], " ")[[1]][1], 
                           species = paste0(strsplit(spp[i], " ")[[1]][2],"*"),
                           ext = ext1, geo = T, removeZeros = T)
  
}

lapply(gBIF, colnames)
sapply(gBIF, function(x) class(x[,"key"]))
str(gBIF[[3]])

# convert to character
gBIF2 <- lapply(gBIF, function(x){
  
  x[,"key"] <- as.character(x[,"key"])
  x
  
})

sapply(gBIF2, function(x) class(x[,"key"]))
str(gBIF[[3]])

## combine all species
occGBIF <- dplyr::bind_rows(gBIF2)

w.xls(occGBIF) # check
occGBIF$id <- sprintf("gbif%05d", 1:nrow(occGBIF))


## get common columns to combine with above data sets
cols <- c("id", "family", "species", "country",  "verbatimLocality", "year", "lat", "lon")

gbif <- occGBIF[,cols]
colnames(gbif) <- c("id", "family", "species","country", "locality","year", "lat", "lon") 

## Merge with above from BIEN
table(occ$is_introduced)
table(occ$native_status)
table(occ$native_status_reason)

occ$id <- sprintf("bien%05d", 1:nrow(occ))
occ$date <- lubridate::as_date(occ$date_collected)
occ$year <- lubridate::year(occ$date) 

str(occ)
colnames(occ)
cols.occ <- c("id", "scrubbed_family", "scrubbed_species_binomial","country", 
              "locality","year", "latitude", "longitude")

bien <- occ[,cols.occ]
colnames(bien) <- c("id", "family", "species","country", "locality","year", "lat", "lon")

allOcc <- merge(bien, gbif, all = T)
head(allOcc)

nrow(gbif) + nrow(bien) == nrow(allOcc)

## Data paper on palms (data not stored on github, but is available here:)
## DOWNLOAD supp info ("WAMAZONIAN_PALMS_data_files_20190618") from this paper: and place in data folder
# Balslev, H., Kristiansen, S. M., & Muscarella, R. (2019). Palm community transects and soil properties in western Amazonia. Ecology, 100(12), e02841. https://doi.org/10.1002/ecy.2841

ps <- list.files("data/WAMAZONIAN_PALMS_data_files_20190618",full.names = T)

ps.spp <- read.csv(ps[2], stringsAsFactors = F)
ps.tran <- read.csv(ps[4], stringsAsFactors = F)
ps.sub <- read.csv(ps[3], stringsAsFactors = F)

str(ps.spp)

ps <- merge(ps.spp, ps.tran, by = "Transect")
head(ps)
ps$tax <- paste(ps$Genus, ps$Species)
spp %in% unique(ps$tax)

ps$id <- sprintf("waps%05d", 1:nrow(ps))

ps$country <- sub("([[:alpha:]]),.*", "\\1", ps$Place_name)

## get sub id coords
ps$subID <- paste(ps$Transect, ps$Subunit, sep = "_")
ps.sub$subID <- paste(ps.sub$Transect, ps.sub$Subunit, sep = "_")
colnames(ps.sub)
colnames(ps.sub)[3:4] <- c("lat1", "lon1")

ps <- merge(ps, ps.sub[,c("subID", "lat1", "lon1")], by = "subID", all.x = T)

ps.ss <- subset(ps, tax %in% spp)
head(ps.ss)

## update coords with subunit coords
ps.ss$Latitude[!is.na(ps.ss$lat1)] <- ps.ss$lat1[!is.na(ps.ss$lat1)]
ps.ss$Longitude[!is.na(ps.ss$lon1)] <- ps.ss$lon1[!is.na(ps.ss$lon1)]

ps.ss <- ps.ss[!duplicated(ps.ss[,c("tax", "Longitude", "Latitude")]),]
head(ps.ss)


ps.sf <- st_as_sf(ps.ss, coords = c("Latitude","Longitude"))
plot(ps.sf[,"Species"])

ps.ss$family <- NA
head(ps.ss)

## get common columns again for paper data
cols.ps <- c("id", "family", "tax", "country",  "Place_name", "Year", "Latitude", "Longitude")
ps.merge <- ps.ss[,cols.ps]
colnames(ps.merge) <- c("id", "family", "species","country", "locality","year", "lat", "lon")

## Merge again
allOcc <- merge(ps.merge, allOcc, all = T)
head(allOcc)
sum(is.na(allOcc$lat))

## convert to Spatial again
sum(is.na(allOcc$lat))

occPalm.sf <- st_as_sf(allOcc, coords = c("lon", "lat"), crs = wgs)
occPalm.sf

occPalm.sir <- st_transform(occPalm.sf, crs = sirgas)
st_write(occPalm.sir, "gis/s_sirgas/occPalm.shp", delete_layer = T)

plot(occPalm.sir[,"species"])

## save (or continue below... )

rm(allOcc, occ, occ.gen, occGBIF, ps, tax)

##### 2 Prepare predictors and occurrence points for model ########

library(sf)
library(raster)
library(dplyr)

source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")
source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/GIS/adjExt.r")
source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/SDM/filter_pts.r")

dir("gis")

## Palm occurrence records are filtered by biomes east of andes, can use terrestrial ecoregions by wwf for this. 
## https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

## or digitised and combined from Stotz. 

## load biomes - digitised from Stotz - or use Wwf biomes - available online and substitute for bm
# combined from Stotz (map2)
# Amazonia Sur             Amazonia Norte y Tepuis  Bosque Alt?ntico         Caatinga                
# Cerrado                  Chaco                    Norte de Am?rica del Sur Pampas 

## load biomes spatial object into bm
# bm <- st_read("gis/s_wgs/eastAndes_diss_sm.shp") # dissolved and smoothed.

ext <- st_bbox(bm)

bm.ext.wgs <- adjExt(st_bbox(bm), d = 0.5, outF= "Extent")
bm.ext.wgs
bm.ext.poly <- ext.poly <- st_sf(data.frame(geometry = st_as_sfc(st_bbox(bm.ext.wgs, crs = wgs))))

bm.sir <- st_transform(bm, crs = sirgas)
plot(st_geometry(bm.sir))

st_crs(bm.sir)$proj4string
st_crs(occPalm.sir)$proj4string

# Get palm species in these biomes
palms_in_biom <- st_intersects(occPalm.sir, bm.sir)
plot(st_geometry(occPalm.sir), col = lengths(palms_in_biom)+1, pch = 16, add = T)

palms.use <- occPalm.sir[lengths(palms_in_biom) > 0, ]
plot(palms.use[,1], col= "black", pch = 16, add = T)
table(palms.use$species)

## Get predictors - climate and terrain metrics

## 2.1 Climate  #####
## download worldclim v2 files and save to  wrldclm folder within data folder
# http://worldclim.com/version2

# get files 
wc.fn <- list.files(file.path("data/wrldclm"), pattern = "\\.tif$", full.names = T)
wc.fn
wc.stck <- stack(wc.fn)
wc.stck

# crop 
wc.stck.crp <- crop(wc.stck, bm.ext.wgs)
wc.stck.crp
plot(wc.stck.crp[[1]])

## project and crop to projected extent
# template raster
aoi.ext.sir <- adjExt(adjExt(extent(bm), d = 0.5, projTo = sirgas, projFrom = wgs), d = 5000)
# st_write(ext2pol(aoi.ext.sir, crs = sirgas.br.crs), "gis/s_sirg/palm_aoi2.shp", delete_layer = T)

r.sir <- raster(aoi.ext.sir, crs = sirgas, res =1000)
r.sir

raster::beginCluster()
wc.sir <- projectRaster(wc.stck.crp, to = r.sir, method = "bilinear")
raster::endCluster()

names(wc.sir)
names(wc.sir) <- gsub(" ", "0", sprintf("bio%02s", 1:19))
wc.names <- names(wc.sir)

wc.sir$bio01


# make mask
aoi.msk <- rasterize(bm.sir, r.sir)
plot(aoi.msk)

## mask to projected aoi
wc.sir <- mask(wc.sir, aoi.msk) # 
wc.sir

## 2.2 DEM ####
## Download DEM data and place in data/dem folder, rescaled to 1km
#  eg https://www.usgs.gov/core-science-systems/eros/coastal-changes-and-impacts/gmted2010?qt-science_support_page_related_con=0#qt-science_support_page_related_con
dem <- raster("data/dem/gmted1k_sirg.tif")
dem

dem <- crop(dem, aoi.msk)
dem <- mask(dem, aoi.msk)

# calculate terrain metrics
terr <- terrain(dem, opt =c("slope", "aspect", "roughness"), unit = "degrees")

## TWI
# dynatopmodel::upslope.area
system.time(
  twi <- dynatopmodel::upslope.area(dem, atb = T)
)
twi
# $atb is the twi
# elapsed 250 s

## Make eastness and northness
Nss <- cos(terr$aspect* pi / 180) # "Northness (aspect)"
Ess <- sin(terr$aspect* pi / 180)  # eastness


stck <- stack(wc.sir, dem, terr, twi, Nss, Ess)
names(stck)

sNames <- names(stck)

sNames[c(20, 24,25,26,27)] <- c("dem", "upslope", "twi", "Nss", "Ess")

names(stck) <- sNames

# # Thin species to one occurrence point per predictor raster cell
palms.thin <- filter_pts(r.sir, palms.use, byID = "species")
table(palms.thin$species)
st_write(palms.thin, "gis/s_sirgas/palmsThin.shp")

cat(paste(sNames, collapse = '", "'))
sNames <- c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "dem", "roughness", "slope", "aspect", "upslope", "twi", "Nss", "Ess")

stck <- brick("gis/r_sirg/palmModStck.tif")
names(stck)
names(stck) <- sNames
stck

#### Make table of palm species for SUPP INFO
palms.use
as.data.frame.table(table(palms.use$species))
w.xls(as.data.frame.table(table(palms.use$species)))
w.xls(as.data.frame.table(table(palms.thin$species)))

### 3. Do palm models ###############


library(sf)
library(raster)
library(dplyr)
library(ENMeval)
library(maxnet)

sNames <- c("bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "dem", "roughness", "slope", "aspect", "upslope", "twi", "Nss", "Ess")

palms.use
table(palms.use$species)

plot(palms.thin[,"species"])
table(palms.thin$species)

## Pairs with predictors
names(stck)
pairs(stck, maxpixels = 500)


# lose aspect, Ess . And these due to correlations
pred.Out <- c("aspect", "twi", "Nss", "Ess", "upslope", "roughness", "bio03", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio13", "bio14", "bio16", "bio17", "bio18", "bio19")
ind.Out <- which(names(stck) %in% pred.Out)
stck.fin <- dropLayer(stck, ind.Out)
stck.fin
stck.fin[[1]]
names(stck.fin) # "bio01" "bio02" "bio04" "bio12" "bio15" "dem"   "slope" FINAL PREDICTORS PALMS
pairs(stck.fin, maxpixels= 500)
plot(stck.fin)
rm(ind.Out, pred.Out)

# do scaling first for extraction (and prediction later)
stck.sc <- scale(stck.fin)

# writeRaster(stck.sc, filename = "gis/r_sirgas/palmModStck_sc.tif", overwrite = T)
stck.sc <- brick("gis/r_sirgas/palmModStck_sc.tif")
stck.sc
names(stck.sc) <- c("bio01", "bio02", "bio04", "bio12", "bio15", "dem","slope")

plot(stck.sc)

## Check plots of occurrence points
splt <- split(palms.thin, palms.thin$species)

chs <- lapply(names(splt), function(x) {
  
  tmp <- st_sf(geometry = st_buffer(st_convex_hull(st_union(splt[[x]])),dist = 10000))
  tmp$species <- x
  tmp
  
})

chs
chulls <- do.call(rbind, chs)
 
unique(palms.use$country)
SA <- c("COL", "VEN", "ECU", "PER", "BRA", "ARG", "PAR", "URY", "BOL", "GUY", "GUF", "SUR")
 
 
data("wrld_simpl", package = "maptools")
wrld <- st_as_sf(wrld_simpl)
wrld
sa.smpl <- wrld[wrld$ISO3 %in% SA,]

sa.smpl.sir <- st_transform(sa.smpl, sirgas)
plot(sa.smpl.sir)

par(mfrow = c(3,3))
for(sp in chulls$species) {

  plot(st_geometry(sa.smpl.sir), main = sp)
  plot(chulls[chulls$species == sp,], add = T)
}


# Get pseudoabsences just from within convex hulls
# dismo::randomPoints() # only with raster mask

n <- 10000
set.seed(999)
rp <- st_sample(chulls, size = rep(n, nrow(chulls)), type = "random")

rp <- st_sf(geometry = rp)
rp$species <- rep(chulls$species, each = n)
rp

png("palm_hulls_absence_pres.png", height = 200, width = 250, units = "mm", res = 100)
par(mfrow = c(3,3))
for(sp in chulls$species) {
  
  plot(st_geometry(sa.smpl.sir), main = sp)
  plot(chulls[chulls$species == sp,], add = T)
  plot(rp[rp$species == sp,], add = T, pch = ".", cex = 0.5, col = "grey90")
  plot(st_geometry(palms.thin[palms.thin$species == sp,]), add = T, col = "red", pch = 16, cex = 0.2)
}
dev.off()

# Do maxent models for each palm species separately

# Get values for presences and absences
rp$pa <- 0
rp$id <- NA
palms.thin$pa <- 1

rp

## Absence points
abs.coords <- cbind(data.frame(rp), st_coordinates(rp))
abs.coords$geometry <- NULL
head(abs.coords)

## presence points
pres.pts <- cbind(data.frame(palms.thin), st_coordinates(palms.thin))
pres.pts$geometry <- NULL
pres.pts <- subset(pres.pts, species != "Attalea funifera")
head(pres.pts)

# # don't use Attalea funifera - see distribution

## get spp names
spp <- as.character(unique(pres.pts$species))

## use parallel within ENMeval 
nCores <- parallel::detectCores()-2

mxRes5 <- lapply(spp, function(sp) {
            
          ENMevaluate(occ = subset(pres.pts, species == sp, select = c("X", "Y")),
                        env = stck.sc,
                        bg.coords = subset(abs.coords, species == sp, select = c("X", "Y")),
                        method = "randomkfold",
                        RMvalues = seq(0.5, 4, 0.5),
                        fc = c("L", "LQ", "H", "LQH", "LQP", "LQHP", "LQHPT"),
                        algorithm = "maxnet",
                        kfolds = 5,
                        parallel = T,
                        numCores = nCores,
                        rasterPreds=FALSE)
            
})

lapply(mxRes5, function(x) x@results)

lapply(mxRes5, function(x) max(x@results[, "avg.test.AUC"]))

best.res <- lapply(mxRes5, function(x) x@results[which.max(x@results[, "avg.test.AUC"]),])
best.res

# into data frame
best.res <- cbind(species = spp, do.call(rbind, best.res))
best.res

w.xls(best.res) # saved to results file

## Do full best models
library(foreach)
library(doParallel)

## Make NewData ####
## get newdata 

## Predict just in convex hull area, and crop to aoi
chulls <- subset(chulls, species != "Attalea funifera") # not in macaw range

mskList <- lapply(split(chulls, chulls$species), function(x) {
  
  tmp <- raster::mask(stck.sc, mask = x)
  tmp <- raster::crop(tmp, ext.r)
  
  } )
names(mskList)

mskList <- mskList[names(mskList) != "Attalea funifera"]

save(mskList, file = "palmMskList.rdata")
load("palmMskList.rdata")

plot(mskList[[3]]$bio01)

vals1 <- lapply(mskList, values)
sapply(vals1, nrow)
nD.len <- nrow(vals1[[1]])

# remove NAs
tmp2 <- lapply(vals1, function(x) {
  
  indNA <- complete.cases(x)
  tmp <- x[indNA, ]
  
  return(list(newData = tmp, indNA = indNA))
  
})

newData <- lapply(tmp2, function(x) x$newData)
indNA <- lapply(tmp2, function(x) x$indNA)

sapply(indNA, function(x) sum(!x))

head(newData[[1]])
head(indNA[[1]])


nCores <- length(spp)

cl <- makeCluster(nCores)
registerDoParallel(cl)


sp <- spp[1]

mxFull <- 
  
  foreach(sp = spp,
          .combine = list,
          .multicombine = TRUE,
          .packages = c("raster", "maxnet")) %dopar% {
            
            
            occ = subset(pres.pts, species == sp, select = c("X", "Y"))
            bg.coords = subset(abs.coords, species == sp, select = c("X", "Y"))
            pa <- c(rep(1, nrow(occ)), rep(0, nrow(bg.coords)))
            
            data <- data.frame(raster::extract(stck.sc, rbind(occ, bg.coords)))
            # remove NAs
            datNA <- complete.cases(data)
            #sum(!indNA)
            data <- data[datNA,]
            pa <- pa[datNA]
            # table(pa)
            
            mx <- maxnet::maxnet(p = pa, data = data,
                                 f = maxnet.formula(p=pa, data = data,
                                                    classes = tolower(best.res[best.res$species == sp, "features"])),
                                 regmult = best.res[best.res$species == sp, "rm"])
            
            #head(newData[[sp]])
            #dim(newData[[sp]])
            pred <- predict(mx, newdata = newData[[sp]], type = "cloglog", clamp = TRUE)
            
            return(list(mx = mx, pred = pred))
          }

stopCluster(cl)

names(mxFull)

mxPred.L <- lapply(mxFull, function(x) x$pred)

## Convert back to rasters - add NAs first
mxPred.r <- mapply(function(x,y){
  
  r <- raster(ext.r, res = 1000, crs = sirgas)
  
  vals <- vector(length = nD.len)
  vals[y] <- x
  r[] <- vals
  r
  
}, mxPred.L, indNA)

plot(mxPred.r[[1]])
names(mxPred.r) <- spp

##convert NAs to 0 to sum (without thresholding low values)
mxPred0 <- lapply(mxPred.r, function(x) {
  x[is.na(x)] <- 0
  x
})

plot(mxPred0[[1]])

plot(stack(mxPred0[[1]], mxPred.r[[1]]), colNA = "black")

palmMaps0 <- stack(mxPred0)
palmMaps0_sum <- sum(palmMaps0)
palmMaps0_sum[palmMaps0_sum==0] <- NA # 0s back to NAs

plot(palmMaps0_sum)

palmMaps <- stack(mxPred.r)
palmMaps
names(palmMaps)

names(palmMaps) <- spp
plot(palmMaps)

# Add up predictions for all palms

# get thresholds

x <- spp[1]

tQ <- lapply(spp, function(x) {
  quantile(extract(mxPred.r[[x]], palms.thin[palms.thin$species == x,]), probs = c(0, 0.01, 0.05, 0.1, 0.15,0.2,1), na.rm = T)}
)

names(tQ) <- spp
tQ[[sp]]

# 0%         1%         5%        10%        15%        20%       100% 
# 0.02704618 0.03642626 0.09078014 0.24295290 0.38681103 0.45461240 0.99995583 

# apply % threshold
mxPred.msk <- lapply(spp, function(x) reclassify(mxPred.r[[x]], rcl = c(0,tQ[[x]]["10%"],NA)))
names(mxPred.msk) <- spp
palmMaps_msk <- stack(mxPred.msk)

plot(stack(mxPred.r[[1]], mxPred.msk[[1]]), colNA = "black")

# r <- mxPred.msk[[1]]
# r[is.na(r)] <- 0
# plot(stack(mxPred.msk[[1]], r), colNA= "black")

# Convert all NAs to 0 so that can be summed
mxPred.msk0 <- lapply(mxPred.msk, function(x) {
  x[is.na(x)] <- 0
  x
  })
names(mxPred.msk0) <- spp

palmMaps_msk0 <- stack(mxPred.msk0)
plot(palmMaps_msk0)

palmMaps_sum <- sum(palmMaps_msk0)

# return 0 to NA
palmMaps_sum[palmMaps_sum ==0] <- NA

par(mfrow = c(1,1))
plot(palmMaps_sum) # this is with 10% threshold

plot(stack(palmMaps0_sum, palmMaps_sum))

save(palmMaps, palmMaps0_sum, palmMaps_sum, file = "data/palmsRasters.rdata")
