#---------------------------------------#
# Step 1 of the migration assessment ####
#---------------------------------------#

# need to create an RStudio new project within the speicifc folder, so this file knows the working directory!

# Organize data, find problem points, then...
# Use Net Squared displacement to identify the start and
# end dates of spring and fall migration
library(rgdal)
library(raster)
library(dplyr)

# identify the name of the folder for this specific assessment
# It only should have a folder named GPS_data in it with the data.

#load up GPS collar database
# data_sp <- readOGR(paste(getwd(),"GPS_data",sep="/"),layer='GPS_data_ready') ###Commented out by Ben 4/21/17.
# GPS_data_ready are the data that result from this script. Need to read in 'fldr' data

###fldr <- readline("What's the name of the folder, yo:") #Change by Ben
###data_sp <- readOGR(paste(getwd(),"GPS_data",sep="/"),layer=fldr)

###data_sp <- readOGR(paste(getwd(),"GPS_data",sep="/"),layer=fldr)

joshFolderTesting<-'D:\\Documents\\Dropbox\\collarProcessingApp\\data\\sampleData'
# joshFolderTesting<-"C:\\Users\\Josh\\Dropbox\\collarProcessingApp\\data\\sampleData"
data_sp<-readOGR(dsn = joshFolderTesting ,
        layer = "codyElk10",
        # layer = "Cody_Elk",
        verbose = FALSE)

head(data_sp@coords)

#Before anything else, MUST make sure that we are in UTMs

######---- JOSH TO DO... HOW WILL WE HANDLE OTHER PROJECTION IMPORTS?
## WILL THE FUNCTION BELOW REPROJECT ANYTHING?
data_sp<-spTransform(data_sp,CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
head(data_sp@coords)

#get date column into POSIXct format
head(data_sp)

###---- JOSH TO DO...
##--- here we'll select the date and ID columns using dropdowns in TAB2
id.col <- readline("Hey yo hit me up with the name of that id column:")
date.col <- readline("What is the name of the date column,dawg:")
# data_sp$D___T__L <- as.POSIXct(strptime(data_sp$D___T__L, format = "%Y-%m-%d %H:%M:%S"), tz ="MST")
data_sp@data$id <- data_sp@data[,id.col]
data_sp@data$date <- data_sp@data[,date.col]

###data_sp$date <- as.POSIXct(strptime(data_sp$date, format = "%m-%d-%Y %H:%M:%S"), tz ="MST") ##jg
data_sp$date <- as.POSIXct(strptime(data_sp$date2, format = "%m-%d-%Y %H:%M:%S"), tz ="MST")  ##jg

#organize and reduce the columns
proj <- proj4string(data_sp)  #grab the proj4string from the shapefile
data <- as.data.frame(coordinates(data_sp))
names(data) <- c("x","y")
 data$id <- data_sp$id
 data$date <- data_sp$date
str(data)
head(data)
rm(data_sp)

# figure out fix rates. Dif is in hours
data <- data[order(data$id, data$date),] #order database first
dif <- c(diff(as.numeric(data$date)/3600),NA) ###Normally diff produces seconds, this converts to hours
hist(dif[dif < 25 & dif > -1])
hist(dif[dif < 10 & dif > -1])
#could look at fix rates during migration months
data$month <- as.numeric(strftime(data$date, format = "%m", tz = "MST"))
hist(dif[dif < 10 & dif > -1 & data$month %in% 4:5])
hist(dif[dif < 10 & dif > -1 & data$month %in% 10:11])
rm(dif)
Tmax <- 100000 #set Tmax to just a bit more than your max fix rate (i.e., you don't want longer steps in this analysis). In seconds
# Tmax <- 50000
# Tmax <- 8000

#check for duplicates
dup <- duplicated(data[,c("id", "date")])
table(dup)   #this should either be ALL false or just a few TRUEs. If there are many TRUEs, you have a problem Houston
data[dup==TRUE,]
data <- data[dup == FALSE,]
rm(dup)

#check for mortalities and problem points. Updated to start from parent directory using dirname()
source("/R_functions/mort.check.R")
source("/R_functions/mov.param.R")
source("/R_functions/creat.burst.R")
source("/R_functions/find.problem.pts.R")

# identify mortality data (or where the collar was not moving much)
data <- data[order(data$id, data$date),] #order database first
data$burst <- creat.burst(data=data, id = TRUE, Tmax = Tmax)
length(unique(data$burst))
data <- mov.param(data=data, burst=data$burst)
morts <- mort.check(data=data, dist_thresh = 50, time_thresh = 48) #dist is in meters, time is in hrs. confusing but it produces the start and end date of death
head(morts)

#how to remove the mortality data from the database
for(i in 1:nrow(morts)){
  toremove <- data$id == morts$id[i] & data$date >= morts$date_start[i] & data$date <= morts$date_end[i]
  table(toremove)
  data <- data[toremove == FALSE,]
}
data <- data[,c("x","y","id","date")] #reduce back down
rm(toremove, morts, i)

# now look for problem points in regards to speed of travel (i.e., where the collar malfunctioned or was in a back of a truck)
data <- data[order(data$id, data$date),] #order database first
data$burst <- creat.burst(data=data, id = TRUE, Tmax = Tmax) #set quite a bit above your max fix rate
length(unique(data$burst))
problems <- find.problem.pts(data=data, burst=data$burst, speedlim=2) # speedlim is meters/second, 3 m/sec is equivelant to moving about 7 miles per hour
table(problems)
data <- data[problems == FALSE,]
data <- data[,c("x","y","id","date")] #reduce back down
rm(problems, find.problem.pts, mort.check)


#identify migration start and stop times for spring and fall ####
data <- data[order(data$id, data$date),] #order database first
elev <- raster(paste(dirname(getwd()),"/GIS_data_for_Wyoming/elevation90m.img",sep=""))
data_sp <- data
coordinates(data_sp) <- c("x","y")
proj4string(data_sp) <- proj
data$elevation <- extract(elev, spTransform(data_sp,projection(elev)))
rm(data_sp, elev)
data$month <- as.numeric(strftime(data$date, format = "%m", tz = "MST"))
data$sprfal <- ifelse(data$month %in% 2:7,"spring","fall")
data$year <- as.numeric(strftime(data$date, format = "%Y", tz = "MST"))
data$year_bio <- ifelse(data$month == 1, data$year-1, data$year)   #use this bio year so animals can start/end migration fall migration in january
data$jul <- as.numeric(strftime(data$date, format = "%j", tz = "MST"))
data$id_yr_sprfal <- paste(data$id, data$year_bio, data$sprfal, sep="_")
data$burst <- creat.burst(data=data, id = TRUE, Tmax = Tmax)
length(unique(data$burst))
data <- mov.param(data=data, burst=data$burst)

table(data$id_yr_sprfal)
migtime <- data[duplicated(data$id_yr_sprfal)==FALSE,c("id_yr_sprfal","id","year_bio","sprfal")]  #put this in excel
head(migtime)
migtime <- migtime[order(migtime$id, migtime$year_bio),]
###Two columns used to identify through NSD plots where migration starts and ends. just making the column now
migtime$start <- NA
migtime$end <- NA

#loop through the rows of migtime and identify start and end dates of spring and fall migration
# make your figure region very large!
for(i in 1:nrow(migtime)){
  temp <- data[data$id_yr_sprfal==migtime$id_yr_sprfal[i],]

    if(dim(temp)[1]<60){ ###This if statement added by Ben Robb 2/13/17. Cannot use data where there's less than two months
      migtime$start[i] <- NA
      migtime$end[i] <- NA
      {next}
    }

  temp$jul2 <- ifelse(temp$month == 1, temp$jul+max(temp$jul), temp$jul)
  temp <- temp[is.na(temp$speed)==FALSE,]
  temp <- temp[order(temp$date),]
  temp$nsd <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2 #someday i'd like to fix this properly
  temp$displacement <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2) #someday i'd like to fix this properly


  #This doesn't happen often, but sometimes only a handful of
  # unique days. In this case, add NAS and move on
  if(length(unique(temp$jul2)) < 4){
    migtime$start[i] <- NA
    migtime$end[i] <- NA
    {next}
  }


}

###Double check loop that all the migration routes are correct
migtime$start<-as.numeric(migtime$start)
migtime$end<-as.numeric(migtime$end)

for(j in 1:nrow(migtime)){
  temp <- data[data$id_yr_sprfal==migtime$id_yr_sprfal[j],]

  if(dim(temp)[1]<60){ ###This if statement added by Ben Robb 2/13/17. Cannot use data where there's less than two months
    {next}
  }
  temp$jul2 <- ifelse(temp$month == 1, temp$jul+max(temp$jul), temp$jul)
  #This doesn't happen often, but sometimes only a handful of
  # unique days. In this case, add NAS and move on
  if(length(unique(temp$jul2)) < 4){
    migtime$start[j] <- NA
    migtime$end[j] <- NA
    {next}
  }

  temp <- temp[is.na(temp$speed)==FALSE,]
  temp <- temp[order(temp$date),]
  temp$nsd <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2 #someday i'd like to fix this properly
  temp$displacement <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2) #someday i'd like to fix this properly

  if(is.na(migtime$start[j])){
    result<-readline("Is there any migration? Y/N?")
    if(result=="Y"){
      migtime$start[j]<-readline("Provide Julian date of start (if weird or not migratory, put NA):")
      migtime$end[j]<-readline("Provide Julian date of end (if weird or not migratory, put NA):")
    }else{
      {next}
    }}
  ###Should you not follow the first if statement and there are data for migrations
    result<-readline("Are these migrations correct? Y/N?")
    if(result=="N"){
      migtime$start[j]<-readline("Provide Julian date of start (if weird or not migratory, put NA):")
      migtime$end[j]<-readline("Provide Julian date of end (if weird or not migratory, put NA):")
    }else{
      {next}
      }
}#End double check loop


rm(i,j, temp, e, s)

dev.off()

#you may have to do this!
migtime$start <- as.numeric(migtime$start)
migtime$end <- as.numeric(migtime$end)
# migtime$end <- as.numeric(migtime$end)

hist(migtime$start[migtime$sprfal=="spring"])
hist(migtime$start[migtime$sprfal=="fall"])
hist(migtime$end[migtime$sprfal=="spring"])
hist(migtime$end[migtime$sprfal=="fall"])

# classify data as winter, spring, summer, and fall ####
# this also removes individual-years where there is no migration
migtime <- migtime[is.na(migtime$start)==FALSE,]

data <- do.call(rbind, lapply(1:nrow(migtime), function(i){
  temp <- data[data$id_yr_sprfal == migtime$id_yr_sprfal[i],]
  temp$seas <- NA
  if(grepl("fall",migtime$id_yr_sprfal[i])){
    jul2 <- ifelse(temp$month == 1, temp$jul+max(temp$jul), temp$jul)
    temp$seas[jul2 < ifelse(migtime$start[i] < 70,migtime$start[i]+max(temp$jul),migtime$start[i])-1] <- "summer"
    temp$seas[jul2 > ifelse(migtime$end[i] < 70,migtime$end[i]+max(temp$jul),migtime$end[i])+1] <- "winter"
    temp$seas[is.na(temp$seas)==TRUE] <- "fall"
  }else{
    temp$seas[temp$jul < migtime$start[i]-1] <- "winter"
    temp$seas[temp$jul > migtime$end[i]+1] <- "summer"
    temp$seas[is.na(temp$seas)==TRUE] <- "spring"
  }
  return(temp)
}))

data <- data[data$seas %in% c("spring","fall"),]   #removes the seasonal ranges, keeps only migration data
data$mig <- paste(data$id, data$seas, data$year_bio, sep="_")
plot(data$x, data$y)
rm(migtime)

# check for fix rates > every 7 hours.
migs <- unique(data$mig)
fixrates <- do.call(rbind, lapply(1:length(migs), function(i){
  return(data.frame(mig=migs[i], medianfix=median(diff(as.numeric(data$date)[data$mig == migs[i]])/3600),
                    maxfix=max(diff(as.numeric(data$date)[data$mig == migs[i]])/3600)))
}))
fixrates

# if necessary, how to remove migs with really poor fix rates
data <- data[data$mig %in% fixrates$mig[fixrates$medianfix < 7],]

# Take a look at how many of the points are > 7 hours apart
data <- data[order(data$id, data$date),] #order seasbase first
seas <- data[,c("date","id","x","y")]
seas$burst <- creat.burst(data=seas, id = TRUE, Tmax = Tmax)
length(unique(seas$burst))
seas <- mov.param(data=seas, burst=seas$burst)
head(seas)
table(seas$dt > 3600)
table(seas$dt > 3600*7) # the number that are TRUE here are the number that are > 7 hours apart!!!!
rm(fixrates, migs, seas, creat.burst, mov.param)

data <- data[,c("x","y","id","date","month","year","year_bio","jul","mig")]

data_sp <- data
coordinates(data_sp) <- c("x","y")
proj4string(data_sp) <- proj

writeOGR(data_sp, paste(paste(getwd(),"GPS_data", sep="/")), "GPS_data_ready", driver="ESRI Shapefile",overwrite=T)
