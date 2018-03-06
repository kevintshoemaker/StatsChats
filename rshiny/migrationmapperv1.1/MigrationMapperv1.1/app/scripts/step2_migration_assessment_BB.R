#---------------------------------------#
# Step 2 of the migration assessment ####
#---------------------------------------#

# Run Brownian Bridge movement models
# and save the results
#-- library(rgdal)
#-- library(move)
#-- library(raster)
#-- library(snowfall)
#-- library(stringr)
#-- library(igraph)

#make out folder where you want things to be stored
out.dir <- paste(getwd(), "BBs", sep="/")
if(dir.exists(out.dir)==FALSE){
  dir.create(out.dir)
}

#load up the GPS collar data
data <- readOGR(paste(getwd(), "GPS_data", sep="/"), "GPS_data_ready")
data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%S"), tz ="MST")

#create raster to do BBs over #####
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.1, (ext[4]-ext[3])*0.1)
ext <- extend(ext, multiplyers)
rm(multiplyers)
land <- raster(ext)
res(land) <- 50
projection(land) <- proj4string(data)
rm(ext)
plot(extent(land))
plot(data, add=TRUE)

# loop to run id/year/seas BBs ####
migs <- unique(data$mig)
sfInit(parallel = T, cpus = 3)   #must change the cpus
sfExport("data", "land", "migs","out.dir")
sfLibrary(move)
DBBs <- sfClusterApplyLB(1:length(migs), function(i){
  d <- data[data$mig == migs[i],] #subsample the migration route of interest
  if(nrow(d)<31){
    return("Error: Less than 31 points")
  }else{

    d <- d[diff(as.numeric(d$date))/3600 > 0.067,]  #remove points where diff time is less than 4 minutes

    mov <- move(x=coordinates(d)[,1], y=coordinates(d)[,2], time=d$date,
                animal=migs[i],proj=CRS(proj4string(d)))   #create mov object
    # must transform to the move package's projection
    # mov <- spTransform(mov, CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs'))
    # mov <- spTransform(mov,center=T)
    # land <- projectRaster(land, crs=CRS('+proj=longlat +datum=WGS84 +no_defs'))
    # plot(mov,type="b")

    #run the dynBGB function
    DBB <- brownian.bridge.dyn(mov, location.error=20, raster=land, margin=11,
                               windowSize=31)
    # plot(DBB)
    save(DBB, file=paste(out.dir, "/", migs[i],".RData", sep=""))
    return(i)
  }
})
sfStop()
rm(migs)
DBBs

# now create the actual migration shapefiles
#make out folder where you want things to be stored
out.dir <- paste(getwd(),"BBs", sep="/")

DBBs <- dir(out.dir, full.names = FALSE)
DBBs <- grep(".RData",DBBs, value=TRUE)
ids <- str_split_fixed(DBBs,"_",2)[,1]
idsunique <- unique(ids)
# loop to average out within each individual
DBBs <- lapply(1:length(idsunique), function(i){
  fls <- DBBs[ids %in% idsunique[i]]
  if(length(fls)==1){
    load(paste(out.dir, fls, sep="/"))
    return(DBB)
  }else{
    load(paste(out.dir, fls[1], sep="/"))
    BB <- DBB
    for(e in 2:length(fls)){
      load(paste(out.dir, fls[e], sep="/"))
      BB <- BB+DBB
    }
    BB <- BB/length(fls)
    return(BB)
  }
})

#make and save population UD
popUD <- DBBs[[1]]
for(i in 2:length(DBBs)){
  popUD <- popUD+DBBs[[i]]
}
popUD[] <- values(popUD)/sum(values(popUD))
writeRaster(popUD, filename=paste(out.dir, "popUD.img", sep="/"),
            format="HFA", overwrite=TRUE)

#make stopover file
#normalize UD values, like cumsum for values in order of size of UD value, i.e., a 0 turns into a 1
popUD <- getVolumeUD(as(popUD, Class="DBBMM"))
qtl <- quantile(values(popUD)[values(popUD)!=1], probs=.1)
stopovers <- reclassify(popUD, rcl=matrix(c(0,qtl,1,qtl,Inf,NA),2,3, byrow=T))
#plot(stopovers)
#remove patches that are smaller than 2 acres (or three 50km pixels)
clmps <- clump(stopovers)
clmpvals <- na.omit(values(clmps))
clmpvals <- data.frame(table(clmpvals))
clmpvals <- as.numeric(as.character(clmpvals$clmpvals)[clmpvals$Freq < 3])
clmps[is.na(values(clmps))==TRUE] <- -5
stopovers[values(clmps) %in% clmpvals == TRUE] <- NA
stopovers <- rasterToPolygons(stopovers, dissolve=TRUE)
stopovers <- spTransform(stopovers, CRS(proj4string(data)))

#make and save population corridor
popcorrid <- DBBs[[1]]
#normalize UD values, like cumsum for values in order of size of UD value, i.e., a 0 turns into a 1
popcorrid <- getVolumeUD(as(popcorrid, Class="DBBMM"))
popcorrid <- reclassify(popcorrid, rcl=matrix(c(0,.99,1,.99,1,0),2,3, byrow=T))
for(i in 2:length(DBBs)){
  popcorridtemp <- DBBs[[i]]
  popcorridtemp <- getVolumeUD(as(popcorridtemp, Class="DBBMM"))
  popcorridtemp <- reclassify(popcorridtemp, rcl=matrix(c(0,.99,1,.99,1,0),2,3, byrow=T))
  popcorrid <- popcorrid+popcorridtemp
}
#get percent of population using each area
popcorridperc <- popcorrid/length(idsunique)
writeRaster(popcorridperc, filename=paste(out.dir, "popCorridor.img", sep="/"),
            format="HFA", overwrite=TRUE)

# reclassify
# corridors will be turned into a 1, and the others NA.
# the two levels are based on Sawyers' paper
highusecorridor <- reclassify(popcorridperc, rcl=matrix(c(0,.1,0,.1,1,1),2,3, byrow=T))
highusecorridor[values(highusecorridor)==0] <- NA
lowusecorridor <- reclassify(popcorridperc, rcl=matrix(c(0,.000001,0,.000001,1,1),2,3, byrow=T))
lowusecorridor[values(lowusecorridor)==0] <- NA
# convert the raster to a polygon object
highusecorridor <- rasterToPolygons(highusecorridor, dissolve=TRUE)
lowusecorridor <- rasterToPolygons(lowusecorridor, dissolve=TRUE)
#now need to transform them back to our normal projection
highusecorridor <- spTransform(highusecorridor, CRS(proj4string(data)))
lowusecorridor <- spTransform(lowusecorridor, CRS(proj4string(data)))

#write the migration contours shapefiles to file
out.dir <- paste(getwd(),"mig_shapefiles", sep="/")
if(dir.exists(out.dir)==FALSE){
  dir.create(out.dir)
}
#remove current files if there are any
fls <- dir(out.dir, full.names=TRUE)
if(length(fls) > 0){
  for(i in 1:length(fls)){
    file.remove(fls[i])
  }
}
#write out final files
writeOGR(stopovers, out.dir, "stopovers", driver="ESRI Shapefile",overwrite=T)
writeOGR(highusecorridor, out.dir, "high_use_corridor", driver="ESRI Shapefile",overwrite=T)
writeOGR(lowusecorridor, out.dir, "low_use_corridor", driver="ESRI Shapefile",overwrite=T)
