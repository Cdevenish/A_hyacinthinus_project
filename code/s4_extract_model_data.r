
## Scripts for analysis in:

## Multi-decadal land-use impacts across the vast range of an iconic threatened species
## Diversity and Distributions
## Christian Devenish, Alexander C. Lees, Nigel J. Collar, Stuart J. Marsden

## Script s4 Extract predictor values for occurrence points in H macaw land cover model


## 1. Unify predictor names across years
## 2. Rasterise biomes
## 3. create predictor stack
## 4. Scale predictors
## 5 Extract points


#### Extract values per year at occurrence points and background absences 

# setwd("PATH/TO/github/repo/A_hyacinthinus_project")

library(sf)
library(raster)

resFolder <- "results"

sirgas <- 5880
wgs <- 4326


## see mapBiomas_script.js for Google Earth Engine raster preparation
# Save results (tif files) from above script to data/gee

## Load points
load("data/allRecs.rdata") # pts.pres
rm(all.sirgas, allRecs, pts.pres)


# load extents
load("data/extents_aoi.rdata") # aoi, msk.aoi, ext.r, ext.bbox
# rm(msk.aoi, ext.r, ext.bbox)
aoi
plot(msk.aoi)

## Add palm extracts
load("palmsRasters.rdata") # palmMaps0_sum - 
rm(palmMaps, palmMaps_sum)
palmMaps0_sum # in memory
names(palmMaps0_sum) <- "palms"

## Load rasters 
list.files("data/gee", "\\.tif$")

fn <- list.files("data/gee", "\\.tif$", full.names = T)
fn

18*25

# make a list of bricks
allYrs <- lapply(fn, brick)

## Make stack
allYrs <- stack(allYrs)

## 1 Unify names ####

## Organise names of predictors and extract year, type, time etc and make universal names (pred) across all years
bNames <- data.frame(fn = names(allYrs))

bNames$year <- as.integer(sub(".*_classification_(\\d{4}).*", "\\1", bNames$fn))
bNames$type <- sub(".*_classification_(\\d{4})_([[:alpha:]]*)_?.*", "\\2", bNames$fn)
bNames$time <- ifelse(grepl("10y", bNames$fn), "10y", "Pres")
bNames$w <- sapply(regmatches(bNames$fn, regexec("\\d{2}k", bNames$fn)), function(x) if(length(x) ==0) "1k" else x)
# ojo regexec gives list, with 0 lengths for non matches, but unlist() or regexpr give vectors and collapse 0 length non matches

bNames$pred <- apply(bNames[,c("type", "time", "w")], 1, paste0, collapse = "_")
head(bNames)
str(bNames)

unique(bNames$year)
unique(bNames$type)
unique(bNames$time)
unique(bNames$w)

# save refernce
save(allYrs, bNames, file = file.path(resFolder, "rasterData.rdata"))

## Loop through years and only extract relevant raster year for each record
years <- unique(bNames$year)
years # raster 25 years: 1995 - 2019

sort(unique(pts.pres$year))
# 25 years, 1995 - 2020 without 1996
length(unique(pts.pres$year)) 

sum(unique(pts.pres$year) %in% years)
which(!unique(pts.pres$year) %in% years)
unique(pts.pres$year)[which(!unique(pts.pres$year) %in% years)]

pts.years <- paste0("y", years[!years %in% c(1996, 2003)])

### Make and presence absence data 

## General strategy is to loop through list of rasters and read to memory (requires sufficient memory!) 
## - but is much faster than reading from disk

# Create random points first
## Make a mask for all data - should be same for all years and layers
# r <- stack(allYrs[[1]], palmMaps0_sum)
# indNA <- complete.cases(values(r))
# r <- raster(r)
# r[] <- indNA
# r[r==0] <- NA
# r
# plot(r, colNA = "black")
# save(r, indNA, file= "data/maskRaster.rdata")
load("data/maskRaster.rdata")
plot(r, colNA = "black")


### 2. Bring in biomes data #####
## download biome shapefile from mapbiomas webpage (https://mapbiomas.org/en/download) and save in data folder
# biomas <- st_read("data/lm_bioma_250.shp")
# 
# biomas.sir <- st_transform(biomas, crs = sirgas)
# 
# ## rasterise for study area
# plot(msk.aoi)
# biomasR <- raster::rasterize(biomas.sir, msk.aoi, field = "CD_Bioma", update = TRUE, updateValue = "!NA")
# plot(biomasR)
# # biomas          CD_Bioma
# # Amazônia        1
# # Caatinga        2
# # Cerrado         3
# # Mata Atlântica  4
# # Pampa           5
# # Pantanal        6
# 
# freq(biomasR)
# 
# dat <- data.frame(biomas = c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica", "Pampa", "Pantanal"),
#                   ID = 1:6)
# biomasR <- ratify(biomasR)
# rat <- levels(biomasR)[[1]]
# rat <- merge(rat, dat)
# 
# levels(biomasR) <- rat
# 
# biomasR
# freq(biomasR)
#
# save(biomas.sir, biomasR, file = "data/biomas_raster.rdata")
load("data/biomas_raster.rdata")
plot(biomasR)
biomasR

biomasR <- mask(biomasR, r)
plot(biomasR)
names(biomasR) <- "biomas"


#### 3. Make the predictor stack ####

years

# yr <- years[1]

allYrs
head(bNames)

## Rename all raster layers to pred names and put into list of raster stacks by year
stckList <- lapply(years, function(yr) {
  r <- raster::dropLayer(allYrs, names(allYrs)[!grepl(yr, names(allYrs))]) # 18 bands per year
  ## rename rasters for prediction
  names(r) <- bNames$pred[match(names(r), bNames$fn)]
  # # add palms nad biomes # bring all into memory below
  r <- addLayer(r, palmMaps0_sum, biomasR)
  r
})

# Makes names safe for list
names(stckList) <- paste0("y", years)
stckList$y1995
stckList$y1995[[1]]

names(stckList)
names(stckList[[25]])


## 4. Get data for scaling -- #####
library(foreach)
library(doParallel)

nCores <- parallel::detectCores()-3 # 13 should do it in 2 loops
cl <- makeCluster(nCores)
registerDoParallel(cl)

start <- Sys.time()
yearsN <- names(stckList)
# yr <- yearsN[1]

## all 1k vars not used
outVars <- c("forest_10y_1k", "nonForest_10y_1k", "agri_10y_1k", "wetL_10y_1k", "past_10y_1k", "forest_Pres_1k", "nonForest_Pres_1k", "agri_Pres_1k", "wetL_Pres_1k", "past_Pres_1k")

valsList <- foreach(yr = yearsN,
                    .combine = list,
                    .multicombine = TRUE,
                    .packages = c("raster", "sf", "dismo")) %dopar% {
                      
                      year <- as.numeric(sub("y", "", yr))
                      
                      # get stack
                      stck <- stckList[[yr]]
                      stck <- raster::dropLayer(stck, outVars) # drop these not used in model
                      
                      ## read stck into memory to speed up extraction, then delete - 
                      # stck <- raster::readAll(stck) # about 1GB each... 15 mins to read..
                      
                      # get data for scaling 
                      nVals <- values(stck)
                      nVals <- nVals[indNA,] # remove NAs
                      
                      rm(stck)
                      return(nVals)


                    }
stopCluster(cl)

end <- Sys.time()
end - start # 19 mins
rm(end, start)

str(valsList, max.level = 1)
dim(valsList[[1]])

#### scale data

### scale
newData.df <- data.frame(do.call(rbind, valsList), year = rep(years, each = sum(indNA)))
dim(newData.df)
head(newData.df)
cols <- colnames(newData.df)[!colnames(newData.df) %in% c("biomas", "year")]
meanND <- colMeans(newData.df[,cols])
sdND <- apply(newData.df[,cols], 2, sd)

meanND
sdND
save(meanND, sdND, file = file.path(resFolder, "scale_params.rdata"))
rm(newData.df); gc()


### 5. Extract data with scaled version ##### 

cl <- makeCluster(nCores)
registerDoParallel(cl)

set.seed(9999)
n <- 10000 #
# do 250,000 absences, and then randomly select from these. Across all years. 

## this loop get all presence data (specific to year) and all absences data sets

start <- Sys.time()

absList.sc <- foreach(yr = yearsN,
                   .combine = list,
                   .multicombine = TRUE,
                   .packages = c("raster", "sf", "dismo")) %dopar% {
                     
                     year <- as.numeric(sub("y", "", yr))
                     
                     # get stack
                     stck <- stckList[[yr]]
                     
                     ## drop biomas and vars not used in model
                     stck <- dropLayer(stck, c(outVars, "biomas"))
                     
                     ## in each loop, create n/nYears random points, extract
                     abs.pts <- dismo::randomPoints(r, n)
                     
                     ## read stck into memory to speed up extraction, then delete - 
                     stck <- raster::readAll(stck) # about 1.7GB each... 15 mins to read..
                     
                     # scale
                     # names(meanND) == names(stck)
                     stck <- scale(stck, center = meanND, scale = sdND)
                     
                     # add biomas again
                     stck <- addLayer(stck, biomasR)
                     
                     abs.vals <- data.frame(raster::extract(stck, abs.pts), abs.pts, year = rep(year, n))
                     #head(abs.vals)
                     
                     ## presence - only for years
                     pts.yr <- subset(pts.pres, yr == paste0("y", year))
                     
                     if(nrow(pts.yr) != 0){
                       pres.data <- data.frame(pts.yr)
                       pres.vals <- data.frame(pres.data[, c("source", "dataSet", "year", "locality", "country")],
                                               raster::extract(stck, pts.yr), st_coordinates(pts.yr))
                     } else pres.vals <- NULL
                     
                     
                     # get newData for prediction and save
                     nVals <- data.frame(values(stck))
                     nVals <- nVals[indNA,] # remove NAs
                     nVals$biomas <- factor(nVals$biomas, levels = 1:6)
                     
                     rm(stck)
                     
                     return(list(abs.pts = abs.pts, abs = abs.vals, pres = pres.vals, newVals = nVals))
                     
                     
                   }
stopCluster(cl)
end <- Sys.time()
end - start # 20 mins
rm(end, start)

str(absList.sc[[1]], max.level = 2)
str(absList.sc, max.level = 1)

abs.vals.l <- lapply(absList.sc, function(x) x$abs)
abs.pts.l <- lapply(absList.sc, function(x) x$abs.pts)
pres.vals.l <- lapply(absList.sc, function(x) x$pres)

## keep new Data as list
newData_sc <- lapply(absList.sc, function(x) x$newVals)
names(newData_sc) <- paste0("y", years)
head(newData_sc$y1995)
dim(newData_sc$y1995)

save(newData_sc, file = file.path(resFolder, "newData_vals_sc.rdata")) # huge file...

# Check abs and pres points
head(abs.vals.l[[1]])
str(abs.vals.l[[1]])

head(pres.vals.l[1:2])

## combine all points
abs.pts_sc <- do.call(rbind, abs.pts.l)
str(abs.pts_sc)

## combine to single data frame and add year
abs.vals_sc <- data.frame(do.call(rbind, abs.vals.l))
pres.vals_sc <- data.frame(do.call(rbind, pres.vals.l))

abs.vals_sc$biomas <- factor(abs.vals_sc$biomas, levels = 1:6)
pres.vals_sc$biomas <- factor(pres.vals_sc$biomas, levels = 1:6)

head(abs.vals_sc)
str(abs.vals_sc)
head(pres.vals_sc)
# standardise xy names
colnames(pres.vals_sc)[colnames(pres.vals_sc) %in% c("X", "Y")] <- c("x","y")

## Can now extract absences values evenly by years..  10,000 each
save(abs.vals_sc, abs.pts_sc, pres.vals_sc, file = file.path(resFolder, "modVals_sc.rdata")) # 

## save data for plotting
save(stckList, bNames, file = file.path(resFolder, "plotData.rdata"))


