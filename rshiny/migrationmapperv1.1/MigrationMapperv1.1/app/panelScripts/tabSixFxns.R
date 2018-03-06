stepSixFunction<-function(input,output,session){
  print('stepSixHappened')
  if(stepSixHappened){
    return()
  }
  stepSixHappened<<-TRUE
  # START BB FUNCTIONS AND DATA PREP ONCE TAB SIX IS ENTERED
  bbIsRunning<<-FALSE

  observeEvent(input$noRun,{
    print('no')
    toggleModal(session,'bbModal',toggle='close')
  })

  # if an individual/year was changed TRUE (i.e. needs to run bb) to false or NA
  # then those files need to be deleted if they were already created
  fileDeleteHandler<-function(){
    existingBBs<-list.files(individualsBbDir, full.names = FALSE)
    existingBBsFull<-list.files(individualsBbDir, full.names = TRUE)
    seasonalDeletes<-c()
    animalDeletes<-c()
    for(i in 1:nrow(migtime)){
      thisAnimal<-migtime[i,'newUid']
      thisYear<-migtime[i,'year_bio']
      # temp file names for all the BB rds to delete
      tempFileNameSpring<-paste0(thisAnimal,'_','spring','_',thisYear)
      tempFileNameFall<-paste0(thisAnimal,'_','fall','_',thisYear)

      tempFileNameWinter<-paste0(thisAnimal,'_','winter','_',thisYear)

      # tempFileNameWinter1<-paste0(thisAnimal,'_','winter1','_',thisYear)
      # tempFileNameWinter2<-paste0(thisAnimal,'_','winter2','_',thisYear)
      # if this springRun or fallRun is na or TRUE and a file exists, then they
      # need to be deleted
      if(migtime[i,'springRun'] | is.na(migtime[i,'springRun'])){
        seasonalDeletes<-c(seasonalDeletes,tempFileNameSpring)
        # seasonalDeletes<-c(seasonalDeletes,tempFileNameWinter1)
        seasonalDeletes<-c(seasonalDeletes,tempFileNameWinter)
        if(!thisAnimal %in% animalDeletes){
          animalDeletes<-c(animalDeletes,as.character(thisAnimal))
        }
      }
      if(migtime[i,'fallRun'] | is.na(migtime[i,'fallRun'])){
        seasonalDeletes<-c(seasonalDeletes,tempFileNameFall)
        # seasonalDeletes<-c(seasonalDeletes,tempFileNameWinter2)
        seasonalDeletes<-c(seasonalDeletes,tempFileNameWinter)
        if(!thisAnimal %in% animalDeletes){
          animalDeletes<-c(animalDeletes,as.character(thisAnimal))
        }
      }
    }

    existingShapefilesMig<-list.files(shapefilesDirMig, full.names = FALSE)
    existingShapefilesWin<-list.files(shapefilesDirWin, full.names = FALSE)
    existingAveragedIndivis<-list.files(averagedIndividualsDir, full.names = FALSE)

    for(i in 1:length(animalDeletes)){
      thisAnimal<-animalDeletes[i]
      if(paste0(thisAnimal,'_migration','.shp') %in% existingShapefilesMig){
          file.remove(paste0(shapefilesDirMig,'//',thisAnimal,'_migration','.shx'))
          file.remove(paste0(shapefilesDirMig,'//',thisAnimal,'_migration','.shp'))
          file.remove(paste0(shapefilesDirMig,'//',thisAnimal,'_migration','.prj'))
          file.remove(paste0(shapefilesDirMig,'//',thisAnimal,'_migration','.dbf'))

          removeUI(
            selector = sprintf('.shiny-input-container:has(#%s)',paste0(thisAnimal,'_migrationgroupMigration'))
          )

          # delete from polygon holder to so its not mapped
          polygonsHolderMigrations[[paste0(thisAnimal,'_migration')]]<<-NULL
          groupNamesMembersMigration[[paste0(thisAnimal,'_migration')]]<<-NULL
      }

      if(paste0(thisAnimal,'_winter','.shp') %in% existingShapefilesWin){
          file.remove(paste0(shapefilesDirWin,'//',thisAnimal,'_winter','.shx'))
          file.remove(paste0(shapefilesDirWin,'//',thisAnimal,'_winter','.shp'))
          file.remove(paste0(shapefilesDirWin,'//',thisAnimal,'_winter','.prj'))
          file.remove(paste0(shapefilesDirWin,'//',thisAnimal,'_winter','.dbf'))

          # also remove the group selector since its already been rendered on the next tabs
          removeUI(
            selector = sprintf('.shiny-input-container:has(#%s)',paste0(thisAnimal,'_wintergroupWinter'))
          )
          # delete from polygon holder to so its not mapped
          polygonsHolderWinter[[paste0(thisAnimal,'_winter')]]<<-NULL
          groupNamesMembersWinter[[paste0(thisAnimal,'_winter')]]<<-NULL
      }

      if(paste0(thisAnimal,'_winter.rds') %in% existingAveragedIndivis){
        file.remove(paste0(averagedIndividualsDir,'//',thisAnimal,'_winter.rds'))
      }

      if(paste0(thisAnimal,'_migration.rds') %in% existingAveragedIndivis){
        file.remove(paste0(averagedIndividualsDir,'//',thisAnimal,'_migration.rds'))
      }

      if(calculateWinterRange=='FALSE' & length(existingShapefilesWin)>0){
        do.call(file.remove, list(list.files(shapefilesDirWin, full.names = FALSE)))
      }

      winterRasters <- dir(individualsBbDir, full.names = TRUE)
      winterRasters <- grep(".rds",winterRasters, value=TRUE)
      # winterRasters <- grep("winter1|winter2",winterRasters, value=TRUE)
      winterRasters <- grep("winter",winterRasters, value=TRUE)

      if(calculateWinterRange=='FALSE' & length(winterRasters)>0){
        do.call(file.remove, list(winterRasters))
      }



    }




    existingRasters<-list.files(rasterDirectoryIndividuals, full.names = FALSE)
    existingBbs<-list.files(individualsBbDir, full.names = FALSE)
    for(i in 1:length(seasonalDeletes)){

      thisAnimal<-seasonalDeletes[i]

      if(paste0(thisAnimal,'.img') %in% existingRasters){
          file.remove(paste0(rasterDirectoryIndividuals,'//',thisAnimal,'.img'))
          file.remove(paste0(rasterDirectoryIndividuals,'//',thisAnimal,'.img.aux.xml'))
      }

      if(paste0(thisAnimal,'.rds') %in% existingBbs){
          file.remove(paste0(individualsBbDir,'//',thisAnimal,'.rds'))
      }
    }

    # also delete the final, merged corrdiors
    rasterGroupFiles<-list.files(rasterDirectoryGroups, full.names = TRUE)
    if(length(rasterGroupFiles)>0){
      do.call(file.remove, list(list.files(rasterDirectoryGroups, full.names = TRUE)))
      progressTracker$finalMerging<<-FALSE
    }
    # also delete the final, merged corrdiors
    finalCorridorFiles<-list.files(corridorsShapefiles, full.names = TRUE)
    if(length(finalCorridorFiles)>0){
      if(stepNineHappened){
        leafletProxy("finalMap")%>%
          clearShapes()
      }

      do.call(file.remove, list(list.files(corridorsShapefiles, full.names = TRUE)))
      finalpolygonsHolderMigrations<<-list()
    }

    shinyjs::disable("startBBButton")
    delay(1000,
      if(!bbIsRunning){
        bbIsRunning<<-TRUE
        prepDataForBB()
      }
    )
  }

  observeEvent(input$yesRun,{
    toggleModal(session,'bbModal',toggle='close')
    leafletProxy("bbOutputMap")%>%
      clearShapes()

    fileDeleteHandler()
    # do.call(file.remove, list(list.files(shapefilesDir, full.names = TRUE)))
    # do.call(file.remove, list(list.files(rasterDirectory, full.names = TRUE)))
    # do.call(file.remove, list(list.files(individualsBbDir, full.names = TRUE)))
  })



  observeEvent(input$startBBButton,{
    isFinished<-checkForFinishedDates()
    if(isFinished$finished==FALSE){
      modalMessager('Error',paste0('You have not yet selected dates or
      skipped animals in the dataset. You are missing dates for the
      following animals/seasons ',toString(isFinished$missingDates[!is.na(isFinished$missingDates)])))
      return()
    }

    shapefilesDirFilesMig<-list.files(shapefilesDirMig)
    shapefilesDirFilesWin<-list.files(shapefilesDirWin)
    rasterDirectoryFiles<-list.files(rasterDirectoryIndividuals)
    individualsBbDirFiles<-list.files(individualsBbDir)

    shapefilesDirFiles<-length(shapefilesDirFilesMig)+length(shapefilesDirFilesWin)
    rasterDirectoryFiles<-length(rasterDirectoryFiles)
    individualsBbDirFiles<-length(individualsBbDirFiles)
    if(shapefilesDirFiles==0 & rasterDirectoryFiles==0 & individualsBbDirFiles==0){
      if(!bbIsRunning){
        bbIsRunning<<-TRUE
        shinyjs::disable("startBBButton")
        prepDataForBB()
      }
    }
    if(shapefilesDirFiles>0 | rasterDirectoryFiles>0 | individualsBbDirFiles>0){
      print('need to adress existing files')
      toggleModal(session,'bbModal',toggle='open')
      return()
    }
  })

  renderLeafletMap<-function(){
    output$bbOutputMap <- renderLeaflet({
       leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16))%>%
       addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo Base") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
       addTiles(group = "OSM (default)") %>%
       addLayersControl(
         baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
         options = layersControlOptions(collapsed = TRUE))%>%
         setView(lng = -111,lat = 42,zoom = 6)
    })
  }

  prepDataForBB<-function(){
    print('prepDataForBB')
    output$bbErrorsList<-renderUI({
      HTML('<a></a>')
    })
    renderLeafletMap()
    #for each animalYear - caluclate whether its spring, fall, summer or winter
    data<-importedDatasetMaster
    data$seas<-NA

    migTimeLength<-nrow(migtime)

    progressIndicator('Applying Migration Dates.. Please wait','start')
    for(i in 1:migTimeLength){

      temp<-data@data[data@data$id_yr==migtime[i,"id_yr"],]
      thisYear<-migtime[i,"year_bio"]

      temp$seas <- NA

      if(calculateWinterRange){
        if(winterDateHandler=='mean'){
          temp[as.Date(temp$newMasterDate)<as.Date(paste(thisYear,meanSpringStartDate,sep='-'))&
            as.Date(temp$newMasterDate)>=as.Date(paste(thisYear,'02','01',sep='-')),'seas']<-'winter1'
        }
        if(winterDateHandler=='chosen'){
          thisChosenDate<-selectedWinterEnd
          thisChosenDate<-format(thisChosenDate,format='%m-%d')
          temp[as.Date(temp$newMasterDate)<as.Date(paste(thisYear,thisChosenDate,sep='-'))&
            as.Date(temp$newMasterDate)>=as.Date(paste(thisYear,'02','01',sep='-')),'seas']<-'winter1'
        }
      }
      #if there was migration in the spring (eg no 1111-11-11 values) then
      # if(migtime[i,"startSpring"]!='1111-11-11' & migtime[i,"endSpring"]!='1111-11-11'){
      if(!is.na(migtime[i,"springRun"]) & migtime[i,"springRun"]){
        temp[as.Date(temp$newMasterDate)>=as.Date(migtime[i,"startSpring"])&
          as.Date(temp$newMasterDate)<=as.Date(migtime[i,"endSpring"]),'seas']<-'spring'

        if(calculateWinterRange){
          temp[as.Date(temp$newMasterDate)<as.Date(migtime[i,"startSpring"])&
            as.Date(temp$newMasterDate)>=as.Date(paste(thisYear,'02','01',sep='-')),'seas']<-'winter1'
        }

      }

      #if there was migration in the fall (eg no 1111-11-11 values) then
      # if(migtime[i,"startFall"]!='1111-11-11' & migtime[i,"endFall"]!='1111-11-11'){

      if(calculateWinterRange){
        if(winterDateHandler=='mean'){
          temp[as.Date(temp$newMasterDate)>as.Date(paste(thisYear,meanFallEndDate,sep='-'))&
            as.Date(temp$newMasterDate)<=as.Date(paste(thisYear+1,'1','31',sep='-')),'seas']<-'winter2'
        }
        if(winterDateHandler=='chosen'){
          thisChosenDate<-selectedWinterStart
          thisChosenDate<-format(thisChosenDate,format='%m-%d')
          temp[as.Date(temp$newMasterDate)>as.Date(paste(thisYear,thisChosenDate,sep='-'))&
            as.Date(temp$newMasterDate)<=as.Date(paste(thisYear+1,'1','31',sep='-')),'seas']<-'winter2'
        }
      }

      if(!is.na(migtime[i,"fallRun"]) & migtime[i,"fallRun"]){
        temp[as.Date(temp$newMasterDate)>=as.Date(migtime[i,"startFall"])&
          as.Date(temp$newMasterDate)<=as.Date(migtime[i,"endFall"]),'seas']<-'fall'

          if(calculateWinterRange){
            temp[as.Date(temp$newMasterDate)>as.Date(migtime[i,"endFall"])&
              as.Date(temp$newMasterDate)<=as.Date(paste(thisYear+1,'1','31',sep='-')),'seas']<-'winter2'
          }
      }

      data@data[data@data$id_yr==migtime[i,"id_yr"],'seas']<-temp$seas
    }
    progressIndicator('Applying Migration Dates.. Please wait','stop')
    # #removes the seasonal ranges, keeps only migration data
    # removed this because of errors in going back and only having the seasons.
    # probably makes more sense to parition out the data at this point anyway
    # importedDatasetMaster<<-importedDatasetMaster[importedDatasetMaster$seas %in% c("spring","fall"),]
    # #create a new migration column
    data@data$mig <- paste(data@data$newUid, data@data$seas, data@data$year_bio, sep="_")
    data@data$mig2 <- paste(data@data$newUid, data@data$seas, data@data$year_bio, sep="_")

    if(calculateWinterRange){
      data<-data[data$seas %in% c("spring","fall","winter1","winter2"),]
      # data<-data[data$seas %in% c("spring","fall","winter"),]
    }
    if(calculateWinterRange=='FALSE'){
      data<-data[data$seas %in% c("spring","fall"),]
    }


    if(nrow(data)==0){
      thisBbTracker<-list()
      thisMigration<-'CURRENT DATA SELECTION'
      thisBbTracker[[thisMigration]]<-list()
      thisBbTracker[[thisMigration]][['seas']]<-NULL
      thisBbTracker[[thisMigration]][['id_yr']]<-NULL
      thisBbTracker[[thisMigration]][['success']]<-FALSE
      thisBbTracker[[thisMigration]][['error']]<-'Selected animals and dates resulted in no data for analysis'
      saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
      dbbResultsReview()
      return()
    }

    # #create simple subset of all data to use in BB
    data <- data[,c("x","y","newUid","id_yr","newMasterDate","month","year","year_bio","jul","mig","mig2","seas")]

    # notes.. think the different origin errors are coming from here.. this is because
    # the data sent to runbb is different on a rerun.. need to standardize extent
    # to importedDatasetMaster???

    progressIndicator('checking fix rates','start')
    #You can add this part anywhere prior to conducting BB analyses...
    data <- data[order(data$mig, data$newMasterDate),]  #order the database
    dif <- c(as.numeric(diff(as.numeric(data$newMasterDate))),0)/3600  #change in time between points (in hrs)
    dif[c(diff(as.numeric(as.factor(paste(data$mig)))),0) != 0] <- 0 #replace the difs when it switches migration with a 0
    #add a burst column for when there are breaks larger than 7 hours in the data
    data$burst <- ifelse(dif >selectedMaxFixInterval, "bad", "good")
    progressIndicator('checking fix rates','stop')

    # if(nrow(data[data$burst =="bad",])>0){
    #   message<-paste0('You have individuals with gaps > ',selectedMaxFixInterval ,' hours in the data. Given
    #   your specification of max gap in time (see Parameters tab), you have ',nrow(data[data$burst =="bad",]),
    #   ' points that will not be connected with their next point in the Brownian Bridge movement analysis.')
    #   modalMessager('Warning',message)
    # }
    runBrownianBridge(data)
  }




  runBrownianBridge<-function(data){
    print('running bb')
    write.csv(migtime,paste(migtimesDirectory,'migtimes.csv',sep="\\"))
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
    ## is this necessary?
    data$newMasterDate <- as.POSIXct(strptime(data$newMasterDate, format = "%Y-%m-%d %H:%M:%S"), tz =selectedTimezone)



    # averaging by looking in folder contents is dangerous.. considering _ in
    # names etc could break the current logic.. cleaner to add refereneces
    # for all all elements in a list at this point
    # individualsToAverage<<-list()
    # uniqueNames<<-unique(data$id)
    # for(i in 1:length(uniqueNames)){
    #   individualsToAverage[[toString(uniqueNames[i])]]<<-c('')
    # }
    # print('gonna average these ones')
    # print(individualsToAverage)

    # old extent.. this caused errors because when adding/removing individuals
    # from data... this resulted in different extent that could not be added
    # together down the road
    # ext <- extent(data)

    #create raster to do BBs over #####
    ext <- extent(importedDatasetMaster)

    multiplyers <- c((ext[2]-ext[1])*0.2, (ext[4]-ext[3])*0.2)
    ext <- extend(ext, multiplyers)
    rm(multiplyers)
    land <- raster(ext)
    res(land) <- rasterResolution
    projection(land) <- proj4string(data)
    # rm(ext)

    progressIndicator('writing migration routes as point shapefiles','start')
    migs <- unique(data$mig)

    winterMigs <- grep("winter",migs, value=TRUE)
    if(length(winterMigs)>0){
      winterSplit <- str_split_fixed(winterMigs,"_",3)
      colnames(winterSplit)<-c('animal','seas','year')
      winterSplit<-data.frame(winterSplit)
      winterSplit$year<-as.numeric(as.character(winterSplit$year))

      for(i in 1:nrow(winterSplit)){
        thisExistingMig<-winterMigs[i]

        thisAnimal<-winterSplit[i,'animal']
        thisSeas<-winterSplit[i,'seas']
        thisYear<-winterSplit[i,'year']

        # if this is the second half of winter range
        if(thisSeas=='winter1'){
          #then the matching piece is the previous years winter2
          otherHalf<-paste(thisAnimal,'winter2',thisYear-1,sep="_")
          # the new migration name will be named winter with the previous year
          newMig<-paste(thisAnimal,'winter',thisYear-1,sep="_")
          data[data$mig == thisExistingMig,'mig']<-newMig
          # if the other half does exist
          if(otherHalf %in% winterMigs){
            # then set its name to the new mig name
            data[data$mig == otherHalf,'mig']<-newMig
          }
        }
        # if its the first half of a  winter range
        if(thisSeas=='winter2'){
          # then its other half would be next years winter1
          otherHalf<-paste(thisAnimal,'winter1',thisYear+1,sep="_")
          # the new winter range name will be this year
          newMig<-paste(thisAnimal,'winter',thisYear,sep="_")
          # set this years values appropriately
          data[data$mig == thisExistingMig,'mig']<-newMig
          # and if the other half is in next years values
          if(otherHalf %in% winterMigs){
            #set to new migration period name
            data[data$mig == otherHalf,'mig']<-newMig
          }
        }
      }
    }

    data <- data[order(data$mig, data$newMasterDate),]  #order the database


    dawgg<<-data

    migs <- unique(data$mig)

    migsLookup<-data@data[data@data$mig %in% migs,c('mig','id_yr','seas')]
    migsLookup<-unique(migsLookup)
    migsLength<-length(migs)
    for(i in 1:migsLength){
      tempPoints <- data[data$mig == migs[i],] #subsample the migration route of interest
      thisSeas<-tempPoints@data[1,'seas']
      # if(thisSeas=='winter1' | thisSeas=='winter2'){
      #   writeOGR(tempPoints, gpsPointsDirectoryWinter, migs[i], driver="ESRI Shapefile",overwrite=T)
      # }
      if(thisSeas=='winter1' | thisSeas=='winter2'){
        writeOGR(tempPoints, gpsPointsDirectoryWinter, migs[i], driver="ESRI Shapefile",overwrite=T)
      }
      if(thisSeas=='spring' | thisSeas=='fall'){
        writeOGR(tempPoints, gpsPointsDirectoryMigration, migs[i], driver="ESRI Shapefile",overwrite=T)
      }
    }
    progressIndicator('writing migration routes as point shapefiles','stop')

    # loop to run id/year/seas BBs ####
    progressIndicator('running id/year/seas bb','start')
    if(sfIsRunning()){sfStop()}
    sfInit(parallel = T, cpus = numberOfCoresToUse)   #must change the cpus ???
    ## if crash then SFINIT is still running or something and causes
    # another crash when rerun

    sfExport("data", "land", "migs","rasterDirectoryIndividuals","masterWorkingDirectory","workingFilesDirectory","bbMargin","migsLookup","dbbMethod","selectedMaxFixInterval","locationError",'windowSize')

    if(dbbMethod=="TRUE"){
      sfLibrary(move)
    }
    if(dbbMethod=="FALSE"){
      sfLibrary(BBMM)
    }

    DBBs <- sfClusterApplyLB(1:migsLength, function(i){
      # processingInfo<-paste0(sprintf("%.1f",round((i/(migsLength+1)*100),1)),'% ','complete')
      # progressIndicator(processingInfo,'update',i/(migsLength+1))
      d <- data[data$mig == migs[i],] #subsample the migration route of interest



      thisBbTracker<-list()

      thisMigration<-migs[i]
      thisSeason<-migsLookup[migsLookup$mig==thisMigration,'seas']
      thisIdYr<-migsLookup[migsLookup$mig==thisMigration,'id_yr']

      thisBbTracker[[thisMigration]]<-list()
      thisBbTracker[[thisMigration]][['seas']]<-thisSeason
      thisBbTracker[[thisMigration]][['id_yr']]<-thisIdYr

      if(nrow(d)<31){
        thisBbTracker[[thisMigration]][['success']]<-FALSE
        thisBbTracker[[thisMigration]][['error']]<-'Error - Less than 31 points'
        saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
        return("Error: Less than 31 points")
      }else{

      # ----------------------- #
      # ----------------------- #
              # if using the dynamic dbb method
      # ----------------------- #
      # ----------------------- #
      if(dbbMethod=='TRUE'){
        d <- d[diff(as.numeric(d$newMasterDate))/3600 > 0.067,]  #remove points where diff time is less than 4 minutes
        mov <- move(x=coordinates(d)[,1], y=coordinates(d)[,2], time=d$newMasterDate,
                     animal=migs[i],proj=CRS(proj4string(d)))   #create mov object
        mov <- burst(mov, d$burst[1:(nrow(d)-1)])
        DBB <- tryCatch({
          brownian.bridge.dyn(mov,
            location.error=locationError,
            raster=land,
            margin=bbMargin,
            iwindowSize=windowSize,
            burstType="good"
            )
        },
         error = function(cond) {
           thisBbTracker[[thisMigration]][['success']]<-FALSE
           thisBbTracker[[thisMigration]][['error']]<-cond
           saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
           return()
         },
         warning = function(cond) {
           thisBbTracker[[thisMigration]][['success']]<-FALSE
           thisBbTracker[[thisMigration]][['error']]<-cond
           saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
           return()
         }
        )
      }
      # ----------------------- #
      # ----------------------- #
              # if using the regular bb method
      # ----------------------- #
      # ----------------------- #
      if(dbbMethod=='FALSE'){
        # need this??
        # d <- d[diff(as.numeric(d$newMasterDate))/3600 > 0.067,]  #remove points where diff time is less than 4 minutes
        bb <- tryCatch({
          brownian.bridge(
            x=coordinates(d)[,1],
            y=coordinates(d)[,2],
            time.lag=diff(as.numeric(d$newMasterDate)/60),
            area.grid=coordinates(land),
            max.lag=selectedMaxFixInterval*60,
            location.error=locationError
          )
        },
         error = function(cond) {
           thisBbTracker[[thisMigration]][['success']]<-FALSE
           thisBbTracker[[thisMigration]][['error']]<-cond
           saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
           return()
         },
         warning = function(cond) {
           thisBbTracker[[thisMigration]][['success']]<-FALSE
           thisBbTracker[[thisMigration]][['error']]<-cond
           saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
           return()
         }
        )
        DBB<- land
        if(!is.null(bb$probability)){
          DBB[] <- bb$probability
        }
        if(is.null(bb$probability)){
          DBB[] <- NULL
          thisBbTracker[[thisMigration]][['success']]<-FALSE
          thisBbTracker[[thisMigration]][['error']]<-'BB resulted in a null probability grid'
          saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
        }

      } # if the result is not null then write the rdata file
        # this weeds out those that have invalid y limits probably
        # because of big time gaps

      if(!is.null(DBB)){
        thisBbTracker[[thisMigration]][['success']]<-TRUE
        thisBbTracker[[thisMigration]][['error']]<-'Success - '
        saveRDS(thisBbTracker,file=paste0(workingFilesDirectory,"\\",'dbbTracker',"\\",thisMigration,'.rds'))
        # need to fix... not sure why does not work
        individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
        rasterDirectory <- paste(masterWorkingDirectory,"rasters", sep="\\")
        saveRDS(DBB, file=paste0(individualsBbDir,"\\", migs[i],".rds"))

        # individualsToAverage[[thisAnimal]]<<-c(individualsToAverage[[thisAnimal]],migs[i])


        #after line 173, need to do this, so it turns it into a raster prior to writeRaster:
        # checks to see if multiple rasters came from a multipart DDB
        if('layers' %in% names(DBB)){
          if(length(DBB@layers)>1){
            DBB <- sum(DBB)
          }else{
            # DBB <- as(DBB, Class="RasterLayer")
            DBB <- DBB[[1]]
          }
        }
        if(! 'layers' %in% names(DBB)){
          DBB <- DBB[[1]]
        }
        writeRaster(DBB, filename=paste0(rasterDirectoryIndividuals,"\\", migs[i],".img"),format="HFA", overwrite=TRUE)
      }
        return(i)
      }
    })


    progressIndicator('running id/year/seas bb','stop')
    sfStop()

    if(length(DBBs)<1){
      modalMessager('ERROR','You selected data and parameters resulted in no
      output. Check the datasets and try again')
      bbIsRunning<<-FALSE
      return()
    }
    dbbResultsReview()
  }

  dbbResultsReview<-function(){
    ### to do... not seeming to run on first go.. seond time, yese
    bbResults<-list.files(paste0(workingFilesDirectory,'\\','dbbTracker'), full.names = TRUE)
    errorMessages<-list()
    for(i in 1:length(bbResults)){
      thisResult<-readRDS(bbResults[i])
      thisMig<-names(thisResult)
      thisIdYr<-thisResult[[1]]$id_yr
      thisSeas<-thisResult[[1]]$seas
      thisSuccess<-thisResult[[1]]$success
      thisError<-thisResult[[1]]$error
      if(!thisSuccess){
        errorMessages[[thisMig]]<-thisError
      }
      if(!is.null(thisSeas)){
        if(thisSeas=='fall'){
          migtime[migtime$id_yr==thisIdYr,'fallRun']<<-FALSE
        }
        if(thisSeas=='spring'){
          migtime[migtime$id_yr==thisIdYr,'springRun']<<-FALSE
        }
      }
      if(is.null(thisSeas)){
        migtime[migtime$id_yr==thisIdYr,'fallRun']<<-FALSE
        migtime[migtime$id_yr==thisIdYr,'springRun']<<-FALSE
      }
    }

    progressTracker$dbbErrors<<-errorMessages
    progressTracker$migtime<<-migtime

    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))

    do.call(file.remove, list(list.files(paste0(workingFilesDirectory,'\\','dbbTracker'), full.names = TRUE)))
    averageIndividualsMigration()
  }

  averageIndividualsMigration<-function(){
    # now create the actual migration shapefiles
    #make out folder where you want things to be stored
    # masterWorkingDirectory <- paste(getwd(),"BBs", sep="/")
    # make a directory to hold all the individual shapefiles
    progressIndicator('averaging individual migrations','start')
    individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
    DBBs <- dir(individualsBbDir, full.names = FALSE)
    DBBs <- grep(".rds",DBBs, value=TRUE)
    DBBs <- grep("spring|fall",DBBs, value=TRUE)

    progressTracker$migtime<<-migtime
    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))

    ids <- str_split_fixed(DBBs,"_",3)[,1]

    idsunique <- unique(ids)
    if(length(idsunique)==0){
      DBBsMigs<-c()
      progressIndicator('averaging individual migrations','stop')
      averageIndividualsWinter(DBBsMigs)
      return()
    }
    # loop to average out within each individual
    DBBsMigs <- lapply(1:length(idsunique), function(i){
      fls <- DBBs[ids %in% idsunique[i]]
      if(length(fls)==1){
        individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
        tempIndivid<-readRDS(paste(individualsBbDir, fls, sep="\\"))

        # checks to see if multiple rasters came from a multipart DDB
        if('layers' %in% slotNames(tempIndivid)){
          if(length(tempIndivid@layers)>1){
            tempIndivid <- sum(tempIndivid)
          }else{
            # DBB <- as(DBB, Class="RasterLayer")
            tempIndivid <- tempIndivid[[1]]
          }
        }

        tempIndivid@title<-paste0(idsunique[i],'_migration')
        saveRDS(tempIndivid, file=paste(averagedIndividualsDir,'\\', idsunique[i],"_migration",".rds", sep=""))
        return(tempIndivid)
      }else{
        individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
        tempIndivid<-readRDS(paste(individualsBbDir, fls[1], sep="\\"))

        if('layers' %in% slotNames(tempIndivid)){
          if(length(tempIndivid@layers)>1){
            tempIndivid <- sum(tempIndivid)
          }else{
            # DBB <- as(DBB, Class="RasterLayer")
            tempIndivid <- tempIndivid[[1]]
          }
        }
        BB <- tempIndivid
        for(e in 2:length(fls)){
          individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
          tempIndivid<-readRDS(paste(individualsBbDir, fls[e], sep="\\"))

          if('layers' %in% slotNames(tempIndivid)){
            if(length(tempIndivid@layers)>1){
              tempIndivid <- sum(tempIndivid)
            }else{
              # DBB <- as(DBB, Class="RasterLayer")
              tempIndivid <- tempIndivid[[1]]
            }
          }

          BB <- BB+tempIndivid
        }
        BB <- BB/length(fls)
        BB@title<-paste0(idsunique[i],'_migration')
        saveRDS(BB, file=paste(averagedIndividualsDir,'\\', idsunique[i],"_migration",".rds", sep=""))
        return(BB)
      }
    })
    progressIndicator('averaging individual migrations','stop')
    averageIndividualsWinter(DBBsMigs)
  }

  averageIndividualsWinter<-function(DBBsMigs){
    progressIndicator('averaging individual winter ranges','start')
    individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
    DBBs <- dir(individualsBbDir, full.names = FALSE)
    DBBs <- grep(".rds",DBBs, value=TRUE)
    # DBBs <- grep("winter1|winter2",DBBs, value=TRUE)
    DBBs <- grep("winter",DBBs, value=TRUE)

    progressTracker$migtime<<-migtime
    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))

    ids <- str_split_fixed(DBBs,"_",3)[,1]

    idsunique <- unique(ids)
    # loop to average out within each individual
    if(length(idsunique)==0){
      progressIndicator('averaging individual winter ranges','stop')
      DBBsWinter<-c()
      mapWriteIndividuals(DBBsMigs,DBBsWinter)
      return()
    }
    DBBsWinter <- lapply(1:length(idsunique), function(i){
      fls <- DBBs[ids %in% idsunique[i]]
      if(length(fls)==1){
        individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
        tempIndivid<-readRDS(paste(individualsBbDir, fls, sep="\\"))

        # checks to see if multiple rasters came from a multipart DDB
        if('layers' %in% slotNames(tempIndivid)){
          if(length(tempIndivid@layers)>1){
            tempIndivid <- sum(tempIndivid)
          }else{
            # DBB <- as(DBB, Class="RasterLayer")
            tempIndivid <- tempIndivid[[1]]
          }
        }

        tempIndivid@title<-paste0(idsunique[i],'_winter')
        saveRDS(tempIndivid, file=paste(averagedIndividualsDir,'\\', idsunique[i],"_winter",".rds", sep=""))
        return(tempIndivid)
      }else{
        individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
        tempIndivid<-readRDS(paste(individualsBbDir, fls[1], sep="\\"))

        if('layers' %in% slotNames(tempIndivid)){
          if(length(tempIndivid@layers)>1){
            tempIndivid <- sum(tempIndivid)
          }else{
            # DBB <- as(DBB, Class="RasterLayer")
            tempIndivid <- tempIndivid[[1]]
          }
        }
        BB <- tempIndivid
        for(e in 2:length(fls)){
          individualsBbDir <- paste(masterWorkingDirectory,"individBb", sep="\\")
          tempIndivid<-readRDS(paste(individualsBbDir, fls[e], sep="\\"))

          if('layers' %in% slotNames(tempIndivid)){
            if(length(tempIndivid@layers)>1){
              tempIndivid <- sum(tempIndivid)
            }else{
              # DBB <- as(DBB, Class="RasterLayer")
              tempIndivid <- tempIndivid[[1]]
            }
          }

          BB <- BB+tempIndivid
        }
        BB <- BB/length(fls)
        BB@title<-paste0(idsunique[i],'_winter')
        saveRDS(BB, file=paste(averagedIndividualsDir,'\\', idsunique[i],"_winter",".rds", sep=""))
        return(BB)
      }
    })
    progressIndicator('averaging individual winter ranges','stop')

    mapWriteIndividuals(DBBsMigs,DBBsWinter)
  }


  mapWriteIndividuals<-function(DBBsMigs,DBBsWinter){
    numMigs<-(length(DBBsMigs))
    numWinter<-(length(DBBsWinter))
    migsComplete<-FALSE
    winterComplete<-FALSE

    if(numMigs==0 & numWinter==0){
      noResultsGeneratedHandler()
      return()
    }

    progressIndicator('writing individual migrations shapefiles','start')

    existingShapefilesMig<-list.files(shapefilesDirMig, full.names = FALSE)



    for(i in 1:numMigs){

      if(numMigs==0){
        migsComplete<-TRUE
        progressIndicator('writing individual migrations shapefiles','stop')
        # return()
        next
      }

      if(paste0(DBBsMigs[[i]]@title,'.shp') %in% existingShapefilesMig){
        print(paste0('skipping ',DBBsMigs[[i]]@title))
        if(i==numMigs){
            migsComplete<-TRUE
        }
        next
      }




      processingInfo<-paste0(sprintf("%.1f",round((i/(numMigs+1)*100),1)),'% ','complete - writing migration shapefiles')
      progressIndicator(processingInfo,'update',i/(numMigs+1))
      tempIndivid<-DBBsMigs[[i]]
      tempPoly <- getVolumeUD(as(tempIndivid, Class="DBBMM"))
      tempPoly <- reclassify(tempPoly, rcl=matrix(c(0,classificationPercentile,1,classificationPercentile,1,0),2,3, byrow=T))
      tempPoly[values(tempPoly)==0] <- NA
      tempPoly <- rasterToPolygons(tempPoly, dissolve=TRUE)
      if(is.null(tempPoly)){
        if(i==numMigs){
            migsComplete<-TRUE
        }
        next
      }
      tempPoly@data$animalId<-DBBsMigs[[i]]@title
      tempPoly <- spTransform(tempPoly, CRS("+init=epsg:4326"))
      # mapIndividPoly(tempPoly)
      theseBounds<-tempPoly@bbox
      if(i==1){
        minLatMig<<-theseBounds[2,1]
        minLonMig<<-theseBounds[1,1]
        maxLatMig<<-theseBounds[2,2]
        maxLonMig<<-theseBounds[1,2]

        minLatBbs<<-theseBounds[2,1]
        minLonBbs<<-theseBounds[1,1]
        maxLatBbs<<-theseBounds[2,2]
        maxLonBbs<<-theseBounds[1,2]
      }
      if(i>1){
        if(theseBounds[2,1]<minLatMig){minLatMig<<-theseBounds[2,1]}
        if(theseBounds[1,1]<minLonMig){minLonMig<<-theseBounds[1,1]}
        if(theseBounds[2,2]>maxLatMig){maxLatMig<<-theseBounds[2,2]}
        if(theseBounds[1,2]>maxLonMig){maxLonMig<<-theseBounds[1,2]}

        if(theseBounds[2,1]<minLatBbs){minLatBbs<<-theseBounds[2,1]}
        if(theseBounds[1,1]<minLonBbs){minLonBbs<<-theseBounds[1,1]}
        if(theseBounds[2,2]>maxLatBbs){maxLatBbs<<-theseBounds[2,2]}
        if(theseBounds[1,2]>maxLonBbs){maxLonBbs<<-theseBounds[1,2]}
      }
      # if(i==numMigs){
      #   leafletProxy("bbOutputMap") %>%
      #     fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
      #   delay(1000,mapIndividualsBBPage())
      # }
      writeOGR(tempPoly, shapefilesDirMig,DBBsMigs[[i]]@title, driver="ESRI Shapefile",overwrite=T)
      polygonsHolderMigrations[[DBBsMigs[[i]]@title]]<<-tempPoly
      if(i==numMigs){
          migsComplete<-TRUE
      }
    }
    progressIndicator('writing individual migrations shapefiles','stop')
    progressIndicator('writing individual winter range shapefiles','start')
    existingShapefilesWin<-list.files(shapefilesDirWin, full.names = FALSE)
    if(numWinter>0){

      # if(numWinter==0){
      #   # migsComplete<-TRUE
      #   winterComplete<-TRUE
      #   next
      # }

      for(i in 1:numWinter){
        if(paste0(DBBsWinter[[i]]@title,'.shp') %in% existingShapefilesWin){
          print(paste0('skipping ',DBBsWinter[[i]]@title))
          if(i==numWinter){
              winterComplete<-TRUE
          }
          next
        }

        processingInfo<-paste0(sprintf("%.1f",round((i/(numWinter+1)*100),1)),'% ','complete - writing winter range shapefiles')
        progressIndicator(processingInfo,'update',i/(numWinter+1))
        tempIndivid<-DBBsWinter[[i]]
        tempPoly <- getVolumeUD(as(tempIndivid, Class="DBBMM"))
        tempPoly <- reclassify(tempPoly, rcl=matrix(c(0,classificationPercentile,1,classificationPercentile,1,0),2,3, byrow=T))
        tempPoly[values(tempPoly)==0] <- NA
        tempPoly <- rasterToPolygons(tempPoly, dissolve=TRUE)
        # sometimes after reclassifying, null polygons and rasters
        # were created.. needed to break loop here
        if(is.null(tempPoly)){
          if(i==numWinter){
              winterComplete<-TRUE
          }
          next
        }
        tempPoly@data$animalId<-DBBsWinter[[i]]@title
        tempPoly <- spTransform(tempPoly, CRS("+init=epsg:4326"))
        theseBounds<-tempPoly@bbox
        if(i==1){
          minLatWin<<-theseBounds[2,1]
          minLonWin<<-theseBounds[1,1]
          maxLatWin<<-theseBounds[2,2]
          maxLonWin<<-theseBounds[1,2]
        }
        if(i>1){
          if(theseBounds[2,1]<minLatWin){minLatWin<<-theseBounds[2,1]}
          if(theseBounds[1,1]<minLonWin){minLonWin<<-theseBounds[1,1]}
          if(theseBounds[2,2]>maxLatWin){maxLatWin<<-theseBounds[2,2]}
          if(theseBounds[1,2]>maxLonWin){maxLonWin<<-theseBounds[1,2]}
        }
        writeOGR(tempPoly, shapefilesDirWin, DBBsWinter[[i]]@title, driver="ESRI Shapefile",overwrite=T)
        polygonsHolderWinter[[DBBsWinter[[i]]@title]]<<-tempPoly
        if(i==numWinter){
            winterComplete<-TRUE
        }
      }
      progressIndicator('writing individual winter range shapefiles','stop')
    } else{
      winterComplete<-TRUE
      progressIndicator('writing individual winter range shapefiles','stop')
    }


    if(winterComplete & migsComplete){
      leafletProxy("bbOutputMap") %>%
        fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
      delay(1000,mapIndividualsBBPage())
    }


    ###########################
    ###### TAB COMPLETED ######
    ###########################
    updateTabsetPanel(session=session, "appTabPanels",
        selected = 'panel6'
    )


    progressTracker$completed<<-'tab6'
    progressTracker$bbCompleted<<-TRUE

    bbIsRunning<<-FALSE

    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
    stepSevenFunction(input,output,session)
    stepEightFunction(input,output,session)
    stepNineFunction(input,output,session)

    js$enableTab("panel7")
    if(calculateWinterRange){
      js$enableTab("panel8")
    }
    js$enableTab("panel9")

    print('DONE')
    print(progressTracker$dbbErrors)

    if(length(progressTracker$dbbErrors)==0){
      modalMessager('Success','The Brownian Bridge analysis has finished.
      Review the results on the map below. Once you are finished reviewing the
      results, click on the tab labeled "7 - ANIMAL GROUPING" to proceed.')
    }

    if(length(progressTracker$dbbErrors)>0){
      modalMessager('Success',paste0('The Brownian Bridge analysis has finished.
      Brownian Bridge was unable to process ',length(progressTracker$dbbErrors),
      ' migrations. Review the successful results on the map below. Details on
      which individuals produced errors can be found below the map in the text
      section. Once you are finished reviewing the results, click on the tab
      labeled "7 - ANIMAL GROUPING" to proceed. Alternatively, you can go back to
      tab 6 and try adjusting migration dates to rectify the errors'))

      htmlString<-'<br><br><h2>ERRORS</h2>'
      for(i in 1:length(progressTracker$dbbErrors)){
        htmlString<-paste0(htmlString,'<strong>',names(progressTracker$dbbErrors[i]),'</strong><br>')
        # htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]]$message,'</p><br>')
        htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]],'</p><br>')
      }

      output$bbErrorsList<-renderUI({
        HTML(htmlString)
      })
    }
    shinyjs::enable("startBBButton")
  }

  noResultsGeneratedHandler<-function(){

    updateTabsetPanel(session=session, "appTabPanels",
        selected = 'panel6'
    )


    progressTracker$completed<<-'tab6'
    progressTracker$bbCompleted<<-TRUE

    bbIsRunning<<-FALSE

    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))

    if(length(progressTracker$dbbErrors)==0){
      modalMessager('No results generated','The Brownian Bridge analysis has finished.
      No results were generated from your dataset.  ')
    }

    if(length(progressTracker$dbbErrors)>0){
      modalMessager('No results generated',paste0('The Brownian Bridge analysis has finished.
      Brownian Bridge was unable to process ',length(progressTracker$dbbErrors),
      ' migrations. Details on
      which individuals produced errors can be found below in the text
      section.'))

      htmlString<-'<br><br><h2>ERRORS</h2>'
      for(i in 1:length(progressTracker$dbbErrors)){
        htmlString<-paste0(htmlString,'<strong>',names(progressTracker$dbbErrors[i]),'</strong><br>')
        # htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]]$message,'</p><br>')
        htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]],'</p><br>')
      }

      output$bbErrorsList<-renderUI({
        HTML(htmlString)
      })
    }
    shinyjs::enable("startBBButton")
  }

  mapIndividualsBBPage<<-function(){

    leafletProxy("bbOutputMap")%>%
      clearShapes()

    for(i in 1:length(polygonsHolderMigrations)){
      if(length(polygonsHolderMigrations)==0){
        return()
      }
      leafletProxy("bbOutputMap") %>%
        addPolygons(data=polygonsHolderMigrations[[i]],
          fillColor='#8b194e',
          fillOpacity=0.65,
          color='#232928',
          weight=1.5,
          popup=~animalId,
          group=~animalId
          )
    }

    for(i in 1:length(polygonsHolderWinter)){
      if(length(polygonsHolderWinter)==0){
        return()
      }
      leafletProxy("bbOutputMap") %>%
        addPolygons(data=polygonsHolderWinter[[i]],
          fillColor='#2aaaa2',
          fillOpacity=0.65,
          color='#232928',
          weight=1.5,
          popup=~animalId,
          group=~animalId
          )
    }

    groupNames<-names(polygonsHolderMigrations)
    if(length(polygonsHolderWinter)>0){
      groupNames<-c(groupNames,names(polygonsHolderWinter))
    }

    leafletProxy("bbOutputMap") %>%
      addLayersControl(
        overlayGroups = groupNames,
        baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE)
      )

    if(stepSevenHappened){
      mapIndividualsGroupPageMigration()
    }
    if(stepEightHappened){
      mapIndividualsGroupPageWinter()
    }

    if(length(progressTracker$dbbErrors)>0){
      htmlString<-'<br><br><h2>ERRORS</h2>'
      for(i in 1:length(progressTracker$dbbErrors)){
        htmlString<-paste0(htmlString,'<strong>',names(progressTracker$dbbErrors[i]),'</strong><br>')
        # htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]]$message,'</p><br>')
        htmlString<-paste0(htmlString,'<p>',progressTracker$dbbErrors[[i]],'</p><br>')
      }
      output$bbErrorsList<-renderUI({
        HTML(htmlString)
      })
    }

  }

  if(progressTracker$bbCompleted){
    renderLeafletMap()
      if(!existingPolygonsLoaded){
        availableShapefiles <- list.files(shapefilesDirMig, pattern = '.shp')
        if(length(availableShapefiles)==0){
          return()
        }
        progressIndicator('loading existing migration shapefiles.. please wait','start')
        for(i in 1:length(availableShapefiles)){
          processingInfo<-paste0('Loading migration shapefiles - ',sprintf("%.1f",round((i/(length(availableShapefiles)+1)*100),1)),'% ','complete')
          progressIndicator(processingInfo,'update',i/(length(availableShapefiles)+1))
          thisIndividual<-tools::file_path_sans_ext(availableShapefiles[i])
          thisPolygon<-readOGR(dsn = shapefilesDirMig ,layer = thisIndividual)
          polygonsHolderMigrations[[thisIndividual]]<<-thisPolygon
          theseBounds<-thisPolygon@bbox
          if(i==1){
            minLatBbs<<-theseBounds[2,1]
            minLonBbs<<-theseBounds[1,1]
            maxLatBbs<<-theseBounds[2,2]
            maxLonBbs<<-theseBounds[1,2]

            minLatMig<<-theseBounds[2,1]
            minLonMig<<-theseBounds[1,1]
            maxLatMig<<-theseBounds[2,2]
            maxLonMig<<-theseBounds[1,2]
          }
          if(i>1){
            if(theseBounds[2,1]<minLatBbs){minLatBbs<<-theseBounds[2,1]}
            if(theseBounds[1,1]<minLonBbs){minLonBbs<<-theseBounds[1,1]}
            if(theseBounds[2,2]>maxLatBbs){maxLatBbs<<-theseBounds[2,2]}
            if(theseBounds[1,2]>maxLonBbs){maxLonBbs<<-theseBounds[1,2]}

            if(theseBounds[2,1]<minLatMig){minLatMig<<-theseBounds[2,1]}
            if(theseBounds[1,1]<minLonMig){minLonMig<<-theseBounds[1,1]}
            if(theseBounds[2,2]>maxLatMig){maxLatMig<<-theseBounds[2,2]}
            if(theseBounds[1,2]>maxLonMig){maxLonMig<<-theseBounds[1,2]}

          }


          if(i==length(availableShapefiles)){
            existingPolygonsLoaded<<-TRUE
            leafletProxy("bbOutputMap") %>%
              fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
            progressIndicator('loading existing migration shapefiles.. please wait','stop')

            observeEvent(input$bbOutputMap_zoom,{
              if(!tab6PolliesAdded){
                leafletProxy("bbOutputMap") %>%
                  fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
                mapIndividualsBBPage()
                tab6PolliesAdded<<-TRUE
              }

            })
            # stepSevenFunction(input,output,session)
          }
        }



        availableShapefiles <- list.files(shapefilesDirWin, pattern = '.shp')
        if(length(availableShapefiles)==0){
          return()
        }
        progressIndicator('loading existing winter range shapefiles.. please wait','start')
        for(i in 1:length(availableShapefiles)){
          processingInfo<-paste0('Loading winter range shapefiles - ',sprintf("%.1f",round((i/(length(availableShapefiles)+1)*100),1)),'% ','complete')
          progressIndicator(processingInfo,'update',i/(length(availableShapefiles)+1))
          thisIndividual<-tools::file_path_sans_ext(availableShapefiles[i])
          thisPolygon<-readOGR(dsn = shapefilesDirWin ,layer = thisIndividual)
          polygonsHolderWinter[[thisIndividual]]<<-thisPolygon
          theseBounds<-thisPolygon@bbox

          if(theseBounds[2,1]<minLatBbs){minLatBbs<<-theseBounds[2,1]}
          if(theseBounds[1,1]<minLonBbs){minLonBbs<<-theseBounds[1,1]}
          if(theseBounds[2,2]>maxLatBbs){maxLatBbs<<-theseBounds[2,2]}
          if(theseBounds[1,2]>maxLonBbs){maxLonBbs<<-theseBounds[1,2]}

          if(i==1){
            minLatWin<<-theseBounds[2,1]
            minLonWin<<-theseBounds[1,1]
            maxLatWin<<-theseBounds[2,2]
            maxLonWin<<-theseBounds[1,2]
          }
          if(i>1){
            if(theseBounds[2,1]<minLatWin){minLatWin<<-theseBounds[2,1]}
            if(theseBounds[1,1]<minLonWin){minLonWin<<-theseBounds[1,1]}
            if(theseBounds[2,2]>maxLatWin){maxLatWin<<-theseBounds[2,2]}
            if(theseBounds[1,2]>maxLonWin){maxLonWin<<-theseBounds[1,2]}
          }
          if(i==length(availableShapefiles)){
            existingPolygonsLoaded<<-TRUE
            leafletProxy("bbOutputMap") %>%
              fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
            progressIndicator('loading existing shapefiles.. please wait','stop')

            observeEvent(input$bbOutputMap_zoom,{
              if(!tab6PolliesAdded){
                leafletProxy("bbOutputMap") %>%
                  fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
                mapIndividualsBBPage()
                tab6PolliesAdded<<-TRUE
              }

            })
            # stepSevenFunction(input,output,session)
          }
        }
      progressTracker$completed<<-'tab6'
      }
  }
}

#
# dataFolderGps <-"D:\\Documents\\Dropbox\\collarProcessingApp\\data\\dataReady"
# importedDatasetMaster<-readOGR(dsn = dataFolderGps ,layer = "GPS_data_ready",verbose = FALSE)
# writeOGR(data, "D://", "GPS_data_ready", driver="ESRI Shapefile",overwrite=T)



#
#
# for (i in 1:nrow(migtime)){
#   print(i)
#   print(migtime[i,])
# }
