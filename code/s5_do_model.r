
## Scripts for analysis in:

## Multi-decadal land-use impacts across the vast range of an iconic threatened species
## Diversity and Distributions
## Christian Devenish, Alexander C. Lees, Nigel J. Collar, Stuart J. Marsden

## Script s5 Do land use/cover model with H macaw


## 1. Create evaluation data sets using geographic cross validation
## 2. Tune model for all pseudoabsence runs
## 3. Do best tuned model 
## 3.a Make new data by year to use in predictions
## 3.b do yearly predictions
## 4.a Create distance decay raster
## 4.b Average predictions over absence runs
## 5.a Variable importance with maxent in java 
## 5.b Figure - variable importance
## 6.a Extract model areas over biomes
## 6.b Reclassifiy models and extract data by biomes ####
## 7. Figures

# setwd("PATH/TO/github/repo/A_hyacinthinus_project")

library(dplyr)
library(raster)
library(ENMeval)
library(ggplot2)
library(sf)
source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/w.xls.r")

resFolder <- "results"

## using dev version of maxnet:
# Make addsamplestobackground true by default
# By default, presence samples are now added to the background data. This should solve a problem some users
# experienced, where the model was sometimes infeasible and therefore glmnet terminated before reaching the end of
# its regularization path.
# devtools::install_github("mrmaxent/maxnet@v0.1.3")
library(maxnet)
packageVersion("maxnet")
# '0.1.3'

library(ENMeval)
packageVersion("ENMeval")
# '0.3.1'

sirgas <- 5880
wgs <- 4326

# load extents
load("data/extents_aoi.rdata") # aoi, msk.aoi, ext.r, ext.bbox
# rm(msk.aoi, ext.r, ext.bbox)

## load mask raster from all predictors for geographic k folds
load("data/maskRaster.rdata") # r, indNA
r

plot(stack(msk.aoi, r))

# load model data (from s4_1..._v2.r)
load(file.path(resFolder, "modVals_sc.rdata")) # abs.vals_sc, abs.pts_sc, pres.vals_sc # replaces "AllData.rdata"
head(abs.vals_sc)
dim(abs.vals_sc)
dim(abs.pts_sc)
head(pres.vals_sc)
summary(pres.vals_sc)

### make vector of predictor names
# cat(paste(colnames(pres.vals), collapse = '", "'))

## All predictor variables
predVars.all <- c("palms", "biomas", "forest_10y_1k", "nonForest_10y_1k", "agri_10y_1k", "wetL_10y_1k", "past_10y_1k", "forest_10y_30k", "nonForest_10y_30k", "agri_10y_30k", "wetL_10y_30k", "past_10y_30k", "forest_10y_10k", "nonForest_10y_10k", "agri_10y_10k", "wetL_10y_10k", "past_10y_10k", "forest_Pres_1k", "nonForest_Pres_1k", "agri_Pres_1k", "wetL_Pres_1k", "past_Pres_1k", "forest_Pres_30k", "nonForest_Pres_30k", "agri_Pres_30k", "wetL_Pres_30k", "past_Pres_30k", "forest_Pres_10k", "nonForest_Pres_10k", "agri_Pres_10k", "wetL_Pres_10k", "past_Pres_10k")

## take out 1k vars 
predVars <- c("palms", "forest_10y_30k", "nonForest_10y_30k", "agri_10y_30k", "wetL_10y_30k", "past_10y_30k", "forest_10y_10k", "nonForest_10y_10k", "agri_10y_10k", "wetL_10y_10k", "past_10y_10k", "forest_Pres_30k", "nonForest_Pres_30k", "agri_Pres_30k", "wetL_Pres_30k", "past_Pres_30k", "forest_Pres_10k", "nonForest_Pres_10k", "agri_Pres_10k", "wetL_Pres_10k", "past_Pres_10k")

## Check NAs
nrow(pres.vals_sc[!complete.cases(pres.vals_sc[,predVars]),])

pres.vals_sc.sf <- st_as_sf(pres.vals_sc, coords = c("x", "y"), agr = "constant", crs = sirgas)

## rename
thin_modData <- pres.vals_sc
head(thin_modData)

## Check absences for NA
head(abs.vals_sc)
abNA <- complete.cases(abs.vals_sc)
sum(!abNA)


## set up tune parameters
fcc <- c("l", "h", "lq", "lqp", "qph", "qpht", "lqhp", "lqpht") # feature combinations
rm = seq(0.5, 3.5, 0.5) # regularion strength

tune.grid <- expand.grid(classes = fcc, regmult = rm)
tune.grid$tune_grid <- 1:nrow(tune.grid)
tune.grid
nrow(tune.grid)


## set up data sets
set.seed(9)
nAbs <- 10 # no of absences runs

n <- 10000

# number of runs
nAbs*nrow(tune.grid)*4

## Parallelise... make outer loops into lists, then parallel over these
library(foreach)
library(doParallel)

nCores <- detectCores()-2

cl <- makeCluster(nCores)
registerDoParallel(cl)
# i = 1

## 1. Create evaluation data sets using geographic cross validation #####

## pseudoabsence loop- list
dataList <- foreach(i = 1:nAbs,
                    .combine = list,
                    .multicombine = TRUE) %dopar% {
  
  #### Geographic k fold #####
  
  # Random sample of absences
  absData <- abs.vals_sc[sample(1:nrow(abs.vals_sc), size = n),]
  # head(absData)
  
  ## Get validation blocks or folds
  evalBlock <- ENMeval::get.checkerboard2(occ = thin_modData[,c("x", "y")], bg.coords = absData[, c("x", "y")],
                                 env = r,
                                 aggregation.factor = c(5, 10))
  
  kfolds <- c(evalBlock$occ.grp, evalBlock$bg.grp)
  #head(evalBlock)
  # table(evalBlock$occ.grp)
  # table(evalBlock$bg.grp)
  
  # plot(absData[,c("x", "y")], col = evalBlock$bg.grp, asp = 1, pch = 19)
  
  na <- nrow(absData)
  np <- nrow(thin_modData)
  
  tmpData <- rbind(thin_modData[, predVars], absData[, predVars])
  p <- c(rep(1, np), rep(0, na))
  
  # nrow(tmpData) == length(p)
  
  list(kfolds = kfolds, tmpData = tmpData, p = p)
  
}

save(dataList, file = file.path(resFolder, "dataList.rdata"))
# load("dataList.rdata")

str(dataList, max.levels = 2)

lapply(dataList, function(x) table(x$kfolds, useNA = "always"))
lapply(dataList, function(x) summary(x$tmpData))

## number of folds same for all samples, 2 for checkerboard1, 4 for checkerboard2, k for randomk
kks <- seq_along(unique(dataList[[1]]$kfolds))


## 2. Tune model for all pseudoabsence runs ######

# i = 1; j = 1; k=1
# i = 1; j = 1; k=2
## Do nested loop
start <- Sys.time()
evalRes <- foreach(i = 1:nAbs,
                    .combine = rbind) %:%
  foreach(j = 1:nrow(tune.grid),
          .combine = rbind) %:%
      foreach(k = kks,  # seq_along(unique(dataList[[i]]$kfolds)
              .packages = c("maxnet", "dismo", "ecospat"),
              .combine = rbind) %dopar% {
      
      #print(paste("k ....", k))
      ## divide data
      pTrain <- dataList[[i]]$p[dataList[[i]]$kfolds != k]
      dataTrain <- dataList[[i]]$tmpData[dataList[[i]]$kfolds != k, ]
      # table(pTrain)
      
      pTest <- dataList[[i]]$p[dataList[[i]]$kfolds == k]
      dataTest <- dataList[[i]]$tmpData[dataList[[i]]$kfolds == k, ]
      
      mx <- maxnet::maxnet(p = pTrain, data = dataTrain,
                   f = maxnet.formula(p=pTrain, data = dataTrain, classes = tune.grid$classes[j]),
                   regmult = tune.grid$regmult[j])
      
      # formula (lqhpt), corresponding to linear, quadratic, hinge, product and threshold features.
      #plot(mx, type = "cloglog")
      # str(mx, max.level = 1)
      
      pred <- predict(mx, newdata = dataTest, type = "cloglog")
      
      # evaluation stats
      eval <- dismo::evaluate(pred[pTest == 1], pred[pTest ==0])
      auc <- eval@auc
      cor <- eval@cor
      tjur <- base::diff(tapply(pred, pTest, mean, na.rm = T))
      # fit is all predicted values (presences and absences), obs are predicted values of just presences
      boyce <- suppressWarnings(ecospat::ecospat.boyce(pred, pred[pTest ==1], nclass = 0, PEplot=FALSE)$Spearman.cor)
      # warning from pred having two classes. shoujld use inherit in ecospat.boyce()
      #cor <- suppressWarnings(cor(pred, pTest)) 
      
      res <- cbind(nAbs = i, tune_grid = j, k = k, auc = auc, cor = cor, tjur = tjur, boyce = boyce)
      rownames(res) <- NULL
      return(res)
    } # end of cross validation loop

## tune grid of 56, 10 abs runs, 42 mins (without biomas - factor)
end <- Sys.time()
end - start
rm(end, start)

stopCluster(cl)

## Process results here. 

head(evalRes)
save(evalRes, file = file.path(resFolder, "eval_results.rdata"))
# load(file.path(resFolder, "eval_results.rdata"))

# 
evalMean <- data.frame(evalRes) %>%
  group_by(nAbs, tune_grid) %>%
  dplyr::summarise(across(.cols = c(auc, cor, tjur, boyce), .fns = list(mean = mean, sd = sd))) %>%
  left_join(y = tune.grid, by = "tune_grid") %>%
  arrange(nAbs, desc(auc_mean)) %>%
  group_by(nAbs) %>%
  slice_head(n = 5)

head(evalMean)
w.xls(evalMean)

eval_grandMean <- data.frame(evalRes) %>%
  group_by(tune_grid) %>%
  dplyr::summarise(across(.cols = c(auc, cor, tjur, boyce), .fns = list(mean = mean, sd = sd))) %>%
  left_join(y = tune.grid, by = "tune_grid") %>%
  arrange(desc(auc_mean))

head(eval_grandMean)
w.xls(eval_grandMean)

# lengthen and widen 
tuneRes.L <- evalMean %>% 
  tidyr::pivot_longer(cols = contains(c("auc", "cor", "tjur", "boyce")),
                      names_sep = "_",
                      names_to = c("metric", "metricSD"),
                      values_to = "value") %>%
  tidyr::pivot_wider(names_from = metricSD, values_from = value)

head(tuneRes.L)

summary(tuneRes.L)
unique(tuneRes.L$metric)
table(tuneRes.L$metric, useNA = "always")

library(ggplot2)

## tuning plots - supp materials
ggplot(tuneRes.L, aes(x = regmult, y = mean, col = classes))+
  geom_line(aes(group = interaction(nAbs, classes)), alpha = 0.15)+ # use two variables in group... 
  stat_summary(aes(group = classes), fun=mean, geom="line")+
  scale_color_discrete(name = "Feature type")+
  facet_wrap(~metric, scales = "free_y")+
  theme_light()+
  ylab("Metric value")+
  xlab("Regularisation multiplier")+
  theme(strip.text.x = element_text(color = "black"))
ggsave(file.path(resFolder,"plots/evalution_metrics_SI.png"))


best.res <- lapply(split(data.frame(evalMean), evalMean[,"nAbs"]), function(x){

  tmp <- x[order(round(x$auc_mean,2), round(x$tjur_mean, 2), decreasing = TRUE),][1:10,] # [1,]
  tmp

})

## AUC only 
# best.res <- lapply(split(data.frame(evalMean), evalMean[,"nAbs"]), function(x){
#   
#   tmp <- x[order(x$auc_mean, decreasing = TRUE),][1:10,] # [1,] 
#   tmp
#   
# })

best.res


#### 3. Do best model #####

## 3.a Make new data by year #####

load(file.path(resFolder, "newData_vals_sc.rdata"))
# stopCluster(cl)


cl <- makeCluster(nCores)
registerDoParallel(cl)

## Make list of full models - no need to use foreach here - so fast anyway... 
mxFull <- foreach(i = 1:nAbs,
          .combine = list,
          .multicombine= TRUE,
          .packages = c("maxnet"))  %dopar% {

            # make full data for absence loop
            pTrain <- dataList[[i]]$p
            dataTrain <- dataList[[i]]$tmpData

            ## chose from each absence run best tuning
            cls <- as.character(best.res[[i]]$classes[1])
            rg <- best.res[[i]]$regmult[1]
            
            ## use overall best tuning for all
            # cls <- "h"
            # rg <- 1.0
            
            mx <- maxnet::maxnet(p = pTrain, data = dataTrain,
                                 f = maxnet.formula(p=pTrain, data = dataTrain, classes = cls),
                                 regmult = rg)

            return(mx)
          }

str(mxFull, max.level = 1)
save(mxFull, file = file.path(resFolder,"mxFull.rdata"))

stopCluster(cl)

cl <- makeCluster(nCores)
registerDoParallel(cl)

## 3.b do yearly predictions ####
mxPred <- foreach(nD = newData_sc,
                  .combine = list,
                  .multicombine= TRUE) %:%
  foreach(i = 1:nAbs,
          .combine = list,
          .multicombine= TRUE,
          .packages = c("maxnet"))  %dopar% {

 
            pred <- maxnet:::predict.maxnet(mxFull[[i]], newdata = nD, type = "cloglog", clamp = TRUE)

            return(pred)
          }

Sys.time() # ~12 mins

# one minute for 1 run - including making models, .. 
str(mxPred, max.level = 1)
str(mxPred[[1]], max.level = 1)

stopCluster(cl)
gc()

### 4. Average predictions ######

## 4.a decay distance raster

## create raster multiplier as distance decay function from presence points (decreases probability of occurrence 
## with increasing distance from presence points) / 
## Thornhill 2017

# ## create distance raster multiplier
# ## gaussian decay (from aceso.decay) ; https://github.com/tetraptych/aceso/blob/master/aceso/decay.py
# gDecay <- function(x, sigma) exp(-(x^2 / (2.0 * sigma^2)))
# 
# # get coords as sf object for distance calculation
# coords <- st_as_sf(data.frame(coordinates(r)), coords = c("x", "y"), crs = sirgas)
# 
# # get distances (all) and then min to each point

# thin points for distance calc to save time. 
# source("https://raw.githubusercontent.com/Cdevenish/R-Material/master/Functions/SDM/filter_pts.r")
# pts.thin <- filter_pts(r, pts.pres) # thins to same resolution as r
# dist.vals <- st_distance(coords, pts.thin)
# str(dist.vals)
# # get min distance
# min_dist <- apply(dist.vals, 1, min)
# str(min_dist)
# rm(dist.vals); gc()
# save(min_dist, file = "min_dist.rdata")
# load("min_dist.rdata")
# 
# rdist <- raster(r)
# rdist[] <- min_dist
# 
# d.vals <- gDecay(min_dist, sigma = 250000) # 250 km value
# 
# rdecay <- raster(r)
# rdecay[] <- d.vals
# 
# rdecay <- mask(rdecay, r)
# 
# # pts.fun <- function() plot(st_geometry(pts.thin), pch = ".", add = T, col = "black")
# # plot(stack(rdist, rdecay), addfun = pts.fun)
# plot(stack(rdist, rdecay))
# 
# # template raster is r, nas in this is indNA
# dim(newData[[1]])[1] + sum(!indNA) == ncell(r)
# 
# save(rdecay, rdist, file = "data/rdecay.rdata")

load("data/rdecay.rdata")

cl <- makeCluster(nCores)
registerDoParallel(cl)


### 4.b Average predictions over absence runs ####

# m <- mxPred[[1]]
## average predictrions and make into rasters
avPred <- foreach(m = mxPred,
                  .combine = list,
                  .multicombine = TRUE,
                  .packages = "raster") %dopar% {
                    
                    # average predictrions
                    sd <- apply(simplify2array(m, higher = FALSE), 1, sd, na.rm = T)
                    mn <- rowMeans(simplify2array(m, higher = FALSE), na.rm = T)
                    
                    rsd <- r
                    rsd[indNA] <- sd
                    
                    rmn <- r
                    rmn[indNA] <- mn
                    
                    # penalise with decay function
                    rmn.dec <- rmn * rdecay
                    
                    
                    return(list(sd = rsd, mn = rmn, mn.dec = rmn.dec))
                    
                  }

# plot(stack(rsd, rmn, rmn.dec))
Sys.time() # 5 mins
str(avPred, max.level = 2)

stopCluster(cl)

raster::plot(stack(avPred[[5]]$mn, avPred[[10]]$mn, avPred[[15]]$mn, avPred[[25]]$mn), maxpixels = 50000)

pdf(file.path(resFolder,"plots/pred_plot_95_10_19_decay.pdf"))
raster::plot(stack(avPred[[5]]$mn, avPred[[5]]$mn.dec, 
                   avPred[[15]]$mn, avPred[[15]]$mn.dec,
                   avPred[[25]]$mn, avPred[[25]]$mn.dec), maxpixels = 500000, nc = 2)
dev.off()

25/6
years <- 1995:2019
tmpS <- stack(avPred[[1]]$mn.dec, avPred[[5]]$mn.dec, 
      avPred[[10]]$mn.dec, avPred[[15]]$mn.dec,
      avPred[[20]]$mn.dec, avPred[[25]]$mn.dec)
names(tmpS) <- years[c(1, 5, 10, 15, 20, 25)]

tmpS

save(avPred, file = file.path(resFolder,"avPred.rdata"))
rm(mxPred); gc()
# load(file.path(resFolder,"avPred.rdata"))

## export single tifs 

rwMods <- stack(lapply(avPred, function(x) x$mn.dec))
rwMods
names(rwMods) <- paste0("rw", years)
writeRaster(rwMods, bylayer = T, filename = "gis/r_sirgas/rw_mods/rwMod.tif", 
            suffix = "names", datatype = "FLT4S", overwrite = T)


## 5.a Variable importance with maxent java #####

for(i in 1:nAbs) dir.create(file.path(getwd(), resFolder, "mx_results", paste0("res", i)), recursive = T)
dir(file.path(getwd(), resFolder, "mx_results"))

library(dismo)               
dismo::maxent()

best.res

cl <- makeCluster(nCores)
registerDoParallel(cl)

# i = 1
mxMax <- foreach(i = 1:nAbs,
                  .combine = list,
                  .multicombine= TRUE,
                  .packages = c("dismo"))  %dopar% {
                    
                    # make full data for absence loop
                    pTrain <- dataList[[i]]$p
                    dataTrain <- dataList[[i]]$tmpData
                    
                    cls <- as.character(best.res[[i]]$classes[1])
                    rg <- best.res[[i]]$regmult[1]
                    
                    ## or use best across all
                    # cls <- "qph"
                    # rg <- 1.0
                    
                    l <- ifelse(grepl("l", cls), "true", "false")
                    q <- ifelse(grepl("q", cls), "true", "false")
                    p <- ifelse(grepl("p", cls), "true", "false")
                    h <- ifelse(grepl("h", cls), "true", "false")
                    t <- ifelse(grepl("t", cls), "true", "false")
                    
                    args <- c(paste0("beta_lqp=", rg),
                              paste0("beta_hinge=", rg),
                              paste0("beta_threshold=", rg),
                              paste0("linear=",l),
                              paste0("quadratic=",q),
                              paste0("product=",p),
                              paste0("threshold=",t),
                              paste0("hinge=",h))
                    
                    
                    mx <- dismo::maxent(p = pTrain, x = dataTrain,
                                        args = args,
                                        path  = file.path(getwd(), resFolder, "mx_results", paste0("res", i)))
                    return(mx)
                  }


str(mxMax, max.level = 1)
save(mxMax, file= file.path(resFolder,"mxMax_final.rdata"))

# 1 minute

# load(file.path(resFolder,"mxMax_final.rdata"))


mxMax[[1]]
plot(mxMax[[1]])
# 
# pdf("plots/response_x_nAbs_partial.pdf", width = 8, height = 10)
# lapply(mxMax, dismo::response, at = NULL)
# dev.off()

## training AUC
## training AUC

# 491 pts 
# 0.8659 AUC

trainAUC <- lapply(mxMax, function(x) {
  
  data.frame(samples = x@results[rownames(x@results) == "X.Training.samples"],
             trainAUC = x@results[rownames(x@results) =="Training.AUC"])
             
})

trainAUC

## get all var contributions 
str(mxMax[[1]]@results)
dimnames(mxMax[[1]]@results)

varsContr <- paste0(predVars, ".contribution")
varspImp <- paste0(predVars, ".permutation.importance")

varsImp <- lapply(mxMax, function(x) {
  
  data.frame(vars = sub(".contribution", "", varsContr), 
             contribution = x@results[match(varsContr, rownames(x@results))],
             importance = x@results[match(varspImp, rownames(x@results))])
  
})

### 5b Plot variable importance #####

varsImp[[1]]
mxMax[[1]]@results # [rownames(mxMax[[1]]@results) %in% varsContr]

varsImp.df <- data.frame(nAbs = rep(1:nAbs, each = nrow(varsImp[[1]])), do.call(rbind, varsImp))
head(varsImp.df)

varsImp.av <- varsImp.df %>%
  group_by(vars)%>%
  summarise(contribution.mn = mean(contribution),
            contr.sd = sd(contribution),
            importance.mn = mean(importance),
            imp.sd = sd(importance)) %>%
  arrange(desc(importance.mn))%>%
  as.data.frame()

varsImp.av
varsImp.av$vars <- factor(varsImp.av$vars, levels = varsImp.av$vars)

varNames <- gsub("_", " ", varsImp.av$vars[2:21])
varNames <- gsub("forest", "Forest", varNames)
varNames <- gsub("nonForest", "Savanna", varNames)
varNames <- gsub("agri", "Agriculture", varNames)
varNames <- gsub("wetL", "Wetland", varNames)
varNames <- gsub("past", "Pasture", varNames)

varNames <- gsub("10y", "10yr", varNames)
varNames <- gsub("Pres", "Current", varNames)
#varNames <- gsub(" ", "-", varNames)

varNames <- c("Palms", varNames)
varNames

nVar <- 10
varNames <- varNames[1:nVar]

### First panel of plot 
p1 <- varsImp.av %>%
  slice_head(n = nVar) %>%
  ggplot(aes(y = importance.mn, x = vars))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(x = vars, ymin = importance.mn-imp.sd, ymax = importance.mn+imp.sd))+
  scale_x_discrete(labels= varNames)+
  ylab("Mean importance")+
  xlab("")+
  theme_light()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

p1

vars <- varsImp.av$vars[1:nVar]
m <- mxMax[[1]]

## response curves - panel b
# get plotting data from dismo::response - get all response curve data for all abensnce runs
respData <- lapply(seq_along(mxMax), function(m) lapply(varsImp.av$vars, function(v) {
  
  pd <- data.frame(response(mxMax[[m]], var = v, range = "p"))
  pd$var <- v
  pd$nAbs <- m
  pd
}
  ))

str(respData, max.level = 1)
str(respData[[1]], max.level = 1)
respCurve_Data <- do.call(rbind, unlist(respData, recursive = F))

head(respCurve_Data)

# name varNames for labeller in facet
names(varNames) <- varsImp.av$vars[1:nVar]

p2 <- respCurve_Data %>%
  filter(var %in% names(varNames)) %>%
    ggplot(aes(x = V1, y = p))+
    geom_line(aes(group = nAbs), col= "grey20", alpha = 0.20)+ 
    stat_summary(aes(group = var), fun=mean, geom="line")+
    facet_wrap(~var, scales = "free_y", ncol = 2, labeller = as_labeller(varNames))+
    theme_light()+
    ylab("Predicted value")+
    xlab("")+
    theme(strip.text.x = element_text(color = "black"),
          axis.text=element_text(size=10))

p2

ggsave(file.path(resFolder,"plots/response_curves.png"), p2)

library(cowplot)

plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_size = 14,
          rel_widths = c(4,5), axis = "b", greedy = F, align = "none")
ggsave(file.path(resFolder, "plots/response_plots10.png"), width = 300, height = 280, units = "mm")


save(varsImp, trainAUC, file = file.path(resFolder, "varImp.rdata"))


par(mfrow = c(2,5))
sapply(mxMax, plot)
par(mfrow = c(1,1))

## 6.a Extract model areas over biomes #####
##### make thresholded areas and plot areas and biome proportion over time... 

## some years have too few points for 10% threshold... 

table(thin_modData$year, useNA = "always")

## try where > 10
table(thin_modData$year)[table(thin_modData$year) >=10]
yrs <- as.numeric(names(table(thin_modData$year)[table(thin_modData$year) >=10]))
years <- 1995:2019

qList <- vector(length = length(yrs), mode = "list")
names(qList) <- paste0("y", yrs)

# i = 1; y <- yrs[1]

for(y in yrs){
  
  i <- which(y == years)
  extr <- raster::extract(avPred[[i]]$mn.dec, thin_modData[thin_modData$year==y, c("x", "y")])
  q10 <- quantile(extr, probs = c(0.05, 0.1), na.rm = T)
  
  qList[[paste0("y", y)]] <- q10
}


qList

plot(sapply(qList, function(x) x["10%"]))
plot(sapply(qList, function(x) x["5%"]))
qmax10 <- max(sapply(qList, function(x) x["10%"]))

## reclassify and get areas... 
## bring in biomes data (see script s4 for details)
load("data/biomas_raster.rdata")
names(biomasR) <- "biomas"
plot(biomasR)
biomas.sir

library(foreach)
library(doParallel)

nCores <- detectCores()-2
cl <- makeCluster(nCores)
registerDoParallel(cl)

# avP <- avPred[[1]]

## 6b Reclassifiy models and extract data by biomes ####

bmAreas <- foreach(avP = avPred,
                   .combine = list,
                   .multicombine= TRUE,
                   .packages = c("raster"))  %dopar% {
                     
                     
                     # mean prediction
                     m <- avP$mn.dec
                     
                     rc_qmx <- raster::reclassify(m, rcl = c(0, qmax10, 0, qmax10, 1, 1))
                     #plot(rc)
                     # rc.mf <- focal(rc, w = matrix(1, nrow=3, ncol=3), fun = modal)
                     stck <- stack(biomasR, rc_qmn)
                     areas1 <- raster::crosstab(stck, long = TRUE)
                     colnames(areas1) <- c("biomas", "rc", "Freq_mn")
                     
                     stck <- stack(biomasR, rc_qmx)
                     areas2 <- raster::crosstab(stck, long = TRUE)
                     colnames(areas2) <- c("biomas", "rc", "Freq_mx")
                     
                     areas <- merge(areas1, areas2)
                     #head(areas)
                     
                     # get model values
                     stck2 <- stack(biomasR, m)
                     names(stck2) <- c("biomas", "mn")
                     vals <- values(stck2)
                     vals <- vals[complete.cases(vals),]
                     
                     list(rc_qmx=rc_qmx, areas=areas, vals=vals)
                     
                   }

stopCluster(cl)

str(bmAreas, max.level = 1)
save(bmAreas, file = file.path(resFolder,"bmAreas.rdata"))

# load(file.path(resFolder,"bmAreas.rdata"))

## 7 Figures #####


areasL <- lapply(bmAreas, function(x) x$areas)
rcStck_qmx <- stack(lapply(bmAreas, function(x) x$rc_qmx))
head(areasL[[1]])

names(rcStck_qmx) <- paste0("y", years)

valsL <- lapply(bmAreas, function(x) x$vals)


table(thin_modData$year%/%5)

## Plot with ggplot
## prepare raster data
allStack <- stack(rcStck_qmx, rwMods)

coords <- xyFromCell(allStack, seq_len(ncell(allStack)))
df1 <- as.data.frame(values(allStack))
df1 <- cbind(coords, df1)
head(df1)

df1 <- df1[complete.cases(df1),]

## rearrange
df2 <- df1 %>%
  tidyr::pivot_longer(cols = -c(x, y), names_to = c("type","year"), 
                      names_pattern = "([[:alpha:]]{2})(\\d{4})", values_to = "value")

head(df2)

unique(df2$type)
unique(df2$year)

## get biomes for plotting
biomas <- st_read("data/lm_bioma_250.shp")
biomas.sir <- st_transform(biomas, crs = sirgas)

## simplify biomas
biomas.smpl <- st_simplify(biomas.sir, dTolerance = 10000, preserveTopology = TRUE)
plot(st_geometry(biomas.smpl))

biomas.smpl <- subset(biomas.smpl, Bioma != "Pampa")

head(df2)

# set limits by aoi extent
ext.bbox

xlim <- c(ext.bbox["xmin"], ext.bbox["xmax"])
ylim <- c(ext.bbox["ymin"], ext.bbox["ymax"])

p5 <- df2 %>%
  filter(year %in% c("1995", "2007", "2019"), type == "rc", value == 1) %>%
  ggplot(aes(x = x, y = y, col = as.factor(value)))+
  
  geom_sf(data= biomas.smpl, col = "grey60", aes(fill = Bioma), inherit.aes = FALSE)+
  scale_fill_manual(values = grey.colors(6, rev = T), guide = FALSE)+
  
  geom_tile()+
  scale_color_manual(values = c("darkgreen"), guide = FALSE)+
  
  facet_wrap(~year, nrow = 1)+
  theme_light()+
  theme(strip.text.x = element_text(color = "black"))+
  xlab("")+
  ylab("")+
  coord_sf(xlim = xlim, ylim = ylim)


# p5 Main figure 2
ggsave(file.path(resFolder, "plots/rc_mods_3_years.png"), p5, width = 300, height = 200, units = "mm")


p6 <- df2 %>%
  filter(year %in% c("1995", "2007", "2019"), type == "rw") %>%
  ggplot(aes(x = x, y = y, fill = value))+
  
  geom_tile()+
  scale_fill_gradientn(colors = terrain.colors(255, rev = TRUE), name = "Model Value")+
  
  geom_sf(data= biomas.smpl, col = "grey60", bg = NA, inherit.aes = FALSE)+
  
  facet_wrap(~year, nrow = 1)+
  theme_light()+
  theme(strip.text.x = element_text(color = "black"))+
  xlab("")+
  ylab("")+
  coord_sf(xlim = xlim, ylim = ylim)

ggsave(file.path(resFolder, "plots/rw_mods_3_years.png"), p6, width = 300, height = 200, units = "mm")

## export individal models per year to tif
getwd()
names(rcStck_qmx) <- paste0("rc", years)
writeRaster(rcStck_qmx, bylayer = T, filename = "gis//r_sirg/rc_mods/binMod.tif",  # set file paths... 
            suffix = "names", datatype = "INT1U", overwrite = T)

## save final model stacks
save(rcStck_qmx, rwMods, file = file.path(resFolder, "finalMods.rdata"))

areaDf <- data.frame(year = rep(1995:2019, sapply(areasL, nrow)), do.call(rbind, areasL))
head(areaDf)

# Amazônia        1
# Cerrado         3
# Pantanal        6

subset(areaDf, biomas %in% c(1,3,6) & rc == 1)

# change in areas
library(dplyr)

areaDf %>%
  filter(year %in% c(1995, 2019), biomas %in% c(1, 3,6), rc ==1) %>%
  group_by(biomas) %>%
  summarise(change = Freq_mx[year == 2019] - Freq_mx[year == 1995])

areaDf %>%
  filter(year %in% c(1995, 2019), biomas %in% c(1, 3,6), rc ==1) %>%
  group_by(biomas)


## area -- all years
areaDf %>%
  filter(rc ==1) %>%
  group_by(year) %>%
  summarise(area = sum(Freq_mx))%>%
  as.data.frame()
# 
## Main figure 3
p3 <- ggplot(subset(areaDf, biomas %in% c(1,3,6) & rc == 1), aes(x = year, y = Freq_mx, fill= as.factor(biomas)))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_d(option = "E",
                      name = "Range by biome", labels = c("Amazonia", "Cerrado", "Pantanal"))+
  ylab(expression(Area~(km^2)))+
  xlab("Year")+
  scale_y_continuous(labels = scales::comma)+
  theme_light()
p3
ggsave(file.path(resFolder, "plots/range_x_biome_x_year.png"), p3, width = 300, height = 200, units = "mm")


