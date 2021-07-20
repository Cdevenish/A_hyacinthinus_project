
## Scripts for analysis in:

## Multi-decadal land-use impacts across the vast range of an iconic threatened species
## Diversity and Distributions
## Christian Devenish, Alexander C. Lees, Nigel J. Collar, Stuart J. Marsden

## Script s2 Set up modelling area



## Set up study area

# setwd("PATH/TO/github/repo/A_hyacinthinus_project")
library(sf)
options(sf_max.plot=1)

sirgas <- 5880
wgs <- 4326

resFolder <- "results"

## load occurrence points
load(file.path(resFolder, "allRecs.rdata"))

allRecs.wgs <- st_as_sf(allRecs, coords = c("lon", "lat"), crs = wgs)
allRecs.sirgas <- st_transform(allRecs.wgs, crs = sirgas)

plot(st_geometry(allRecs.sirgas))

buff <- st_buffer(st_convex_hull(st_union(allRecs.sirgas)), dist = 25000)
plot(buff, add = T) 


# birdlife shapefile # Birdlife shape file can be requested from Data zone or IUCN red list webpages.
## save the single shapefile for the Hyacinth Macaw range in the data folder.
vl <- st_read("data/Anodorhynchus_hyacinthinus_BLv10_range.shp")
bl <- st_transform(vl, sirgas)
bl <- st_union(bl)

plot(bl)
plot(buff, add = T, border = "blue")
plot(all.occ.sir, add =T)

aoi <- st_buffer(st_convex_hull(st_union(bl, buff)), 25000)
aoi

plot(aoi, add=T, border = "darkred")

ext.bbox <- st_bbox(aoi) %/% 1000 * 1000

ext.r <- raster::extent(ext.bbox[c("xmin", "xmax", "ymin", "ymax")])
ext.r

msk.aoi <- raster(ext.r, res = 1000, crs = sirgas)
msk.aoi
values(msk.aoi) <- 1

msk.aoi <- mask(msk.aoi, as(aoi, "Spatial"))
plot(msk.aoi)
msk.aoi

aoi.wgs <- st_transform(aoi, wgs)

save(aoi, msk.aoi, ext.r, ext.bbox, file = "data/extents_aoi.rdata")

aoi.ext <- st_make_grid(aoi, n = 1)
aoi.ext
