

### TO DO ###

# sp object incorporated 24 May 2016

# 3 
## use more friendly names for this:

# nb <- ld+1 # etc.

# and then use in subscripts

## add a by argument for splitting by factor column in the (sp) dataframe and thinning separately


## Note that thinning is also possible using kernel density methods. ie remove points based on a probability of the kernel
# density at particular point. Check pointthinning java application in "../sdm" - 
# occurrencethinner.zip

### function version ####


minDistPoint <- function(d, xy = c("x","y"), dist){
  
  ## function to eliminate minimum number of points less than dist apart.
  ## duplicate or points within minimum distance will be deleted from first to last row of data frame.
  ## ie order your data frame with those (potential duplicates/within distance) records at start.
  
  # d is sp object SPDF, sf points object or data frame with x, y coordinates (first two columns 
  # default, must be numeric)
  # xy are the x y columns (names or numbers) of data frame
  # dist is minimum distance for points to be apart (in units of x, y). Points equal or less than 
  ## this distance, in projection units (e.g. m)
  
  library(sf)
  require(sp)
  require(spdep)
  
  
  # convert sp SPDF to dataframe
  class.d <- class(d)
  
  if("SpatialPointsDataFrame" %in% class.d){
    sp.d <- d # create backup to subset at end with points to keep
    d <- data.frame(coordinates(d))
    rownames(d) <- rownames(data.frame(sp.d))
    xy <- c(1,2)
    
  }
    
   if("sf" %in% class.d) {
    
    sf.d <- d
    d <- data.frame(st_coordinates(d))
    rownames(d) <- rownames(sf.d)
    xy <- c(1,2)
    
   }
  
  if("data.frame" %in% class.d & length(class.d) == 1) sp.d <- d
  
  if(!any(c("sf", "data.frame", "SpatialPointsDataFrame") %in% class.d)) {
    stop("Data must be data frame, sf class or SpatialPointsDataFrame")
  }
  
  # check that coords are numeric
  if(!all(sapply(d[,xy], is.numeric))){
    stop("X Y coordinates must be numeric")
  }
  
  # original number of rows
  orig.n <- nrow(d)
  
  ## remove identical coordinates
  dups <- duplicated(d[,xy], fromLast = T) # use fromLast so that duplicates are removed 
  # from beginning to end of data frame
  # useful if data is ordered so that those to be deleted are first.
  
  # x <- data.frame(n = c(1,1,1,2,2,3,7,9), k = c("no", "no", "yes", "no", "yes", "yes", "yes", "yes"))
  # duplicated(x$n)
  # x[!duplicated(x$n),]
  # duplicated(x$n, fromLast = T)
  # x[!duplicated(x$n, fromLast = T),]
  
  #sum(dups)
  
  d <- d[!dups,]
  
  md <- dist - 1
  
  # check length of d for temp col names below
  ld <- ncol(d)
  
  ## loop this until minimum distance is < critical distance
  while(md < dist) { # minimum distance > dist for loop to break
    
    # get coord from d
    coords <- as.matrix(d[,xy])
    
    # get nearest neighbour
    knn <- knearneigh(coords, k = 1)
    
    ## convert to nb class
    nb <- knn2nb(knn)
    
    # get dist to nn
    nndist <- nbdists(nb, coords)
    
    # what's min dist?
    md <- min(unlist(nndist))
    
    # combine dist to near neighbour
    ## use these unfriendly names (in terms of readability), but safer in case df already has nn, dist colnames, etc
    #d <- cbind(coords, unlist(nb)) ## near neighbours # was d[, ld+1]
    #d <- cbind(d, unlist(nndist)) ## dist to nn # # was d[, ld+2] but cbind is better for matrix (and data.frame)
    ## swings and roundabouts, with matrix, you can't add columns like this [,ld+1]
    # but you can with data.frame
    # cbind, then creates additional columns, but doesn't replace them.
    
    d[, ld+1] <- unlist(nb)
    d[, ld+2] <- unlist(nndist)
    ## make flag for when a point is deleted
    d[,ld+3] <- T ## flag
    
    ## loop through nn distances, if < dist, then flag for deleting, unless already flagged
    for(i in seq_len(nrow(d))){
      
      if(d[i,ld+3]){ # if point hasn't been flagged already and ... 
        if(d[i,ld+2] <= dist) { # if it is less or equal to distance ...
          d[d[i,ld+1],ld+3] <- F # then flag for deleting
        }
      }
    }
    
    # update dataframe by deleting nearest points (using logical flag)
    d <- d[d[,ld+3],]
  }
  
  ## delete extra cols from d
  d[,(ld+1):(ld+3)] <- list(NULL)
  
  
  if("SpatialPointsDataFrame" %in% class.d){ 
    
    rowsToKeep <- rownames(d)
    d <- sp.d[rownames(data.frame(sp.d)) %in% rowsToKeep,]
  }
  
  
  if("sf" %in% class.d){ 
    
    rowsToKeep <- rownames(d)
    d <- sf.d[rownames(sf.d) %in% rowsToKeep,]
  }
  
  
  
  cat(paste0(sum(dups), " duplicate points removed\n", 
             orig.n - (sum(dups) + nrow(d)), " points within ", dist, " m removed\n",
             nrow(d), " points remaining\n"))
  
  return(d)
}


# ## eg
# d1 <- data.frame(x = runif(1000, 0,100), y = runif(1000,0,100))
# plot(d1)
# d2 <- minDistPoint(d1, dist = 5)
# plot(d2)
# # how many points deleted?
# nrow(d1) - nrow(d2)
