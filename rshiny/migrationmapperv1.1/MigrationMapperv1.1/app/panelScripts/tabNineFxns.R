stepNineFunction<-function(input,output,session){

print("step 9")
if(stepNineHappened){
  return()
}

stepNineHappened<<-TRUE
finalMergingRunning<<-FALSE


# MERGING GROUPS
output$finalMap <- renderLeaflet({
   leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16))%>%
   addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo Base") %>%
   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
   addTiles(group = "OSM (default)") %>%
  #  addLayersControl(
  #    baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
  #    options = layersControlOptions(collapsed = TRUE))%>%
   setView(lng = -111,lat = 42,zoom = 6)
})

finalPolygonsHolder<<-list()

addPolygonsToMap<-function(){
  groupColors<-c('#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
  greens<-c('#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b')
  purps<-c('#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')
  oranges<-c('#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
  reds<-c('#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
  blues<-c('#d0d1e6','#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858')

  finalPolyLength<-length(finalPolygonsHolder)
  if(finalPolyLength==0){
    return()
  }



  progressIndicator('loading polygons.. please wait...','start')




  finalPolyNames<-names(finalPolygonsHolder)


  finalStopovers<-grep("migration_stopovers|winter_core",finalPolyNames, value=TRUE)
  finalHighUse<-grep("winter_high_use|migration_high_use",finalPolyNames, value=TRUE)
  finalMedUse<-grep("migration_med_use_corridor|winter_med_use",finalPolyNames, value=TRUE)
  finalLowUse2<-grep("low_use_2ormore",finalPolyNames, value=TRUE)
  finalLowUse<-grep("migration_low_use_corridor|winter_low_use",finalPolyNames, value=TRUE)
  newOrder<-c(finalLowUse,finalLowUse2,finalMedUse,finalHighUse,finalStopovers)
  tempList<-list()

  for(i in 1:length(newOrder)){
    thisObjName<-newOrder[i]
    tempList[[thisObjName]]<-finalPolygonsHolder[[thisObjName]]
    if(thisObjName %in% finalStopovers){
      tempList[[thisObjName]]$color<-sample(purps, 1)
    }
    if(thisObjName %in% finalHighUse){
      tempList[[thisObjName]]$color<-sample(reds, 1)
    }
    if(thisObjName %in% finalMedUse){
      tempList[[thisObjName]]$color<-sample(oranges, 1)
    }
    if(thisObjName %in% finalLowUse2){
      tempList[[thisObjName]]$color<-sample(blues, 1)
    }
    if(thisObjName %in% finalLowUse){
      tempList[[thisObjName]]$color<-sample(greens, 1)
    }
  }
  rm(finalPolygonsHolder)
  finalPolygonsHolder<<-tempList
  rm(tempList)


  leafletProxy("finalMap")%>%
    clearShapes()

  if(length(finalPolygonsHolder)>0){
    for(i in 1:length(finalPolygonsHolder)){
      thisPolygon<-finalPolygonsHolder[[i]]
      print('mapping')
      print(i)

      leafletProxy("finalMap") %>%
        addPolygons(data=thisPolygon,
          fillColor=thisPolygon$color[1],
          fillOpacity=0.35,
          color='#667a77',
          weight=0.5,
          popup=names(finalPolygonsHolder[i]),
          group=names(finalPolygonsHolder[i])
          )
    }
    leafletProxy("finalMap") %>%
      fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)%>%
      addLayersControl(
        overlayGroups = rev(names(finalPolygonsHolder)),
        baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE)
      )



  }
  progressIndicator('loading polygons.. please wait...','stop')
}

if(progressTracker$finalMerging & length(finalPolygonsHolder)==0){
  filesToMap <- list.files(paste0(masterWorkingDirectory,'\\PopulationShapefiles'), pattern = '.shp')
  if(length(filesToMap)>0){
    progressIndicator('Loading final shapefiles','start')
    for(i in 1:length(filesToMap)){
      processingInfo<-paste0('Loading final shapefiles - ',sprintf("%.1f",round((i/(length(filesToMap)+1)*100),1)),'% ','complete')
      progressIndicator(processingInfo,'update',i/(length(filesToMap)+1))
      thisIndividual<-tools::file_path_sans_ext(filesToMap[i])
      thisPolygon<-readOGR(dsn = paste0(masterWorkingDirectory,'\\PopulationShapefiles') ,layer = thisIndividual,verbose = FALSE)
      finalPolygonsHolder[thisIndividual]<<-thisPolygon
    }
    progressIndicator('Loading final shapefiles','stop')
  }
}

observeEvent(input$finalMap_zoom,{
    if(progressTracker$finalMerging & !tab8PolliesAdded){
      tab8PolliesAdded<<-TRUE
      addPolygonsToMap()
      leafletProxy("groupsMap") %>%
        fitBounds(minLonBbs,minLatBbs,maxLonBbs,maxLatBbs)
    }
})

groupMergeBuilderMigration<<-list()
groupMergeBuilderWinter<<-list()


mergeAnimalsFunction<-function(){
  groupMergeBuilderMigration<<-list()
  groupMergeBuilderWinter<<-list()
  # if an individual did not get a group, we need to asign them to
  #  the nogroup group
  individualsMigration <- list.files(shapefilesDirMig,'.shx')
  for(i in 1:length(individualsMigration)){
    thisIndividual<-individualsMigration[i]
    thisIndividual<-tools::file_path_sans_ext(thisIndividual)
    if(is.na(match(thisIndividual,names(groupNamesMembersMigration)))){
      groupNamesMembersMigration[[thisIndividual]]<<-c('99','noGroup')
    }
  }

  if(is.null(names(groupNamesMembersMigration))){
    animalNamesMigration<-c('allAnimals')
  }

  if(!is.null(names(groupNamesMembersMigration))){
    animalNamesMigration<-names(groupNamesMembersMigration)
  }

  for(i in 1:length(animalNamesMigration)){
    thisAnimal<-animalNamesMigration[i]
    thisGroup<-groupNamesMembersMigration[[thisAnimal]][2]
    groupMergeBuilderMigration[[thisGroup]]<<-c(groupMergeBuilderMigration[[thisGroup]],thisAnimal)
  }

  if(calculateWinterRange==TRUE){
    individualsWinter <- list.files(shapefilesDirWin,'.shx')
    if(length(individualsWinter)>0){
      for(i in 1:length(individualsWinter)){
        thisIndividual<-individualsWinter[i]
        thisIndividual<-tools::file_path_sans_ext(thisIndividual)
        if(is.na(match(thisIndividual,names(groupNamesMembersWinter)))){
          groupNamesMembersWinter[[thisIndividual]]<<-c('99','noGroup')
        }
      }
    }

    if(is.null(names(groupNamesMembersWinter))){
      animalNamesWinter<-c('allAnimals')
    }

    if(!is.null(names(groupNamesMembersWinter))){
      animalNamesWinter<-names(groupNamesMembersWinter)
    }


    for(i in 1:length(animalNamesWinter)){
      thisAnimal<-animalNamesWinter[i]
      thisGroup<-groupNamesMembersWinter[[thisAnimal]][2]
      groupMergeBuilderWinter[[thisGroup]]<<-c(groupMergeBuilderWinter[[thisGroup]],thisAnimal)
    }
  }

  prepareDbbs()
}


observeEvent(input$noMerge,{
  print('no')
  toggleModal(session,'finalMergeModal',toggle='close')
})

observeEvent(input$yesMerge,{
  toggleModal(session,'finalMergeModal',toggle='close')
  leafletProxy("finalMap")%>%
    clearShapes()
  do.call(file.remove, list(list.files(paste0(masterWorkingDirectory,'\\PopulationShapefiles'), full.names = TRUE)))
  do.call(file.remove, list(list.files(paste0(rasterDirectory,'\\groups'), full.names = TRUE)))
  finalMergingRunning<<-TRUE
  shinyjs::disable("mergeAnimalsButton")
  delay(1000,mergeAnimalsFunction())
})

observeEvent(input$mergeAnimalsButton,{
  shinyjs::disable("mergeAnimalsButton")
  if(finalMergingRunning){
    print('running')
    return()
  }
  print('not running')
  filesPresent <- list.files(paste0(masterWorkingDirectory,'\\PopulationShapefiles'), pattern = '.shp')
  if(length(filesPresent)==0){
    # shinyjs::disable("mergeAnimalsButton")
    finalMergingRunning<<-TRUE
    mergeAnimalsFunction()
  }
  if(length(filesPresent)>0 & !finalMergingRunning){
    delay(500,
      shinyjs::enable("mergeAnimalsButton")
    )
    print('need to address existing files')
    toggleModal(session,'finalMergeModal')
  }
})

prepareDbbs<-function(){
  lastOneMig=FALSE
  lastOneWin=FALSE
  lastOneBoth=FALSE

  groupNamesMigration<-names(groupMergeBuilderMigration)

  for(i in 1:length(groupNamesMigration)){
    thisGroup<-groupNamesMigration[i]
    theseAnimals<-groupMergeBuilderMigration[[thisGroup]]
    DBBs <- lapply(1:length(theseAnimals), function(i){
      thisAnimal<-theseAnimals[i]
      fileName<-paste0(thisAnimal,'.rds')
      tempInidivd<-readRDS(paste(averagedIndividualsDir, fileName, sep="\\"))
    })
    if(i==length(groupNamesMigration)){
      lastOneMig=TRUE
      if(calculateWinterRange=='FALSE'){
        lastOneWin=TRUE
      }
      if(lastOneMig & lastOneWin){
        lastOneBoth=TRUE
      }
    }
    thisType='migration'
    createPopulationCorridors(DBBs,thisGroup,thisType,lastOneBoth)
  }

  if(calculateWinterRange){
    groupNamesWinter<-names(groupMergeBuilderWinter)
    for(i in 1:length(groupNamesWinter)){
      thisGroup<-groupNamesWinter[i]
      theseAnimals<-groupMergeBuilderWinter[[thisGroup]]
      DBBs <- lapply(1:length(theseAnimals), function(i){
        thisAnimal<-theseAnimals[i]
        fileName<-paste0(thisAnimal,'.rds')
        tempInidivd<-readRDS(paste(averagedIndividualsDir, fileName, sep="\\"))
      })
      if(i==length(groupNamesWinter)){
        lastOneWin=TRUE
        if(lastOneMig & lastOneWin){
          lastOneBoth=TRUE
        }
      }
      thisType='winter'
      createPopulationCorridors(DBBs,thisGroup,thisType,lastOneBoth)
    }
  }


}





createPopulationCorridors<-function(DBBs,thisGroup,thisType,lastOneBoth){
    # remove spaces from group name (-:
    thisGroup<-gsub("[[:blank:]]", "", thisGroup)

    theseNames<-c()
    if(thisType=='winter'){
      theseNames<-names(groupMergeBuilderWinter)
    }
    if(thisType=='migration'){
      theseNames<-names(groupMergeBuilderMigration)
    }

    groupNameForFiles<-paste0(thisGroup,"_")
    if(thisGroup=='noGroup'){
      groupNameForFiles<-"group1"
      if(groupNameForFiles %in% theseNames){
        groupNameForFiles<-"group1_noName_"
      }
      if(groupNameForFiles %in% theseNames){
        groupNameForFiles<-"group1_noNameNoName_"
      }
    }



    # totalGroupAnimals<-length(DBBs)
    #make and save population utilization dist (UD)
    progressText<-'making population UD'
    if(nchar(thisGroup)>0){
      progressText<-paste0('making population UD',' for group ',thisGroup)
    }

    treats<<-DBBs

    progressIndicator(progressText,'start')
    if(length(DBBs)>1){
      popUD <- DBBs[[1]]
      for(i in 2:length(DBBs)){
        popUD <- popUD+DBBs[[i]]
      }
      popUD[] <- values(popUD)/sum(values(popUD))
    }
    if(length(DBBs)==1){
      popUD <- DBBs[[1]]
      popUD[] <- values(popUD)/sum(values(popUD))
    }

    writeRaster(popUD, filename=paste(rasterDirectoryGroups, paste0(groupNameForFiles,"_",thisType,"_","popUD.img"), sep="/"),
                format="HFA", overwrite=TRUE)
    progressIndicator('making population ids','stop')

    progressText<-'making stopover files'
    if(nchar(thisGroup)>0){
      progressText<-paste0('making stopover files',' for group ',thisGroup)
    }
    progressIndicator(progressText,'start')
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
    stopovers <- spTransform(stopovers, CRS("+init=epsg:4326"))
    progressIndicator('making stopover files','stop')


    progressText<-'creating population corridors'
    if(nchar(thisGroup)>0){
      progressText<-paste0('creating population corridors',' for group ',thisGroup)
    }
    progressIndicator(progressText,'start')

    if(length(DBBs)>1){
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
      #popcorridperc <- popcorrid/length(DBBs)
    }

    if(length(DBBs)==1){
      #make and save population corridor
      popcorrid <- DBBs[[1]]
      #normalize UD values, like cumsum for values in order of size of UD value, i.e., a 0 turns into a 1
      popcorrid <- getVolumeUD(as(popcorrid, Class="DBBMM"))
      popcorrid <- reclassify(popcorrid, rcl=matrix(c(0,.99,1,.99,1,0),2,3, byrow=T))
      #get percent of population using each area
      #popcorridperc <- popcorrid/length(DBBs)
    }

    writeRaster(popcorrid, filename=paste(rasterDirectoryGroups, paste0(groupNameForFiles,"_",thisType,"_","popCorridor.img"), sep="/"),
                format="HFA", overwrite=TRUE)

    popcorridperc <- popcorrid/length(DBBs)

    progressIndicator('creating population corridors','stop')

    # reclassify
    # corridors will be turned into a 1, and the others NA.
    # the two levels are based on Sawyers' paper
    progressText<-'reclassifying corridors'
    if(nchar(thisGroup)>0){
      progressText<-paste0('reclassifying corridors',' for group ',thisGroup)
    }
    progressIndicator(progressText,'start')




    highusecorridor <- reclassify(popcorridperc, rcl=matrix(c(0,0.2,0,0.2,1,1),2,3, byrow=T))
    highusecorridor[values(highusecorridor)==0] <- NA
    if(cellStats(highusecorridor,sum)>0){
      highusecorridor <- rasterToPolygons(highusecorridor, dissolve=TRUE)
      highusecorridor <- spTransform(highusecorridor, CRS("+init=epsg:4326"))
      if(thisType=='migration'){
        writeOGR(highusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","high_use_corridor"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","high_use_corridor")]<<-highusecorridor
      }
      if(thisType=='winter'){
        writeOGR(highusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","high_use"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","high_use")]<<-highusecorridor
      }
    }

    medusecorridor <- reclassify(popcorridperc, rcl=matrix(c(0,.1,0,.1,1,1),2,3, byrow=T))
    medusecorridor[values(medusecorridor)==0] <- NA
    if(cellStats(medusecorridor,sum)>0){
      medusecorridor <- rasterToPolygons(medusecorridor, dissolve=TRUE)
      medusecorridor <- spTransform(medusecorridor, CRS("+init=epsg:4326"))
      if(thisType=='migration'){
        writeOGR(medusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","med_use_corridor"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","med_use_corridor")]<<-medusecorridor
      }
      if(thisType=='winter'){
        writeOGR(medusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","med_use"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","med_use")]<<-medusecorridor
      }

    }

    lowuse2ormore <- reclassify(popcorrid, rcl=matrix(c(0,1.9,0,1.9,Inf,1),2,3, byrow=T))
    lowuse2ormore [values(lowuse2ormore)==0] <- NA
    if(cellStats(lowuse2ormore,sum)>0){
      lowuse2ormore <- rasterToPolygons(lowuse2ormore, dissolve=TRUE)
      lowuse2ormore <- spTransform(lowuse2ormore, CRS("+init=epsg:4326"))
      writeOGR(lowuse2ormore, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","low_use_2ormore"), driver="ESRI Shapefile",overwrite=T)
      finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","low_use_2ormore")]<<-lowuse2ormore
    }


    lowusecorridor <- reclassify(popcorridperc, rcl=matrix(c(0,0.000001,0,0.000001,1,1),2,3, byrow=T))
    lowusecorridor[values(lowusecorridor)==0] <- NA
    if(cellStats(lowusecorridor,sum)>0){
      lowusecorridor <- rasterToPolygons(lowusecorridor, dissolve=TRUE)
      lowusecorridor <- spTransform(lowusecorridor, CRS("+init=epsg:4326"))
      if(thisType=='migration'){
        writeOGR(lowusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","low_use_corridor"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","low_use_corridor")]<<-lowusecorridor
      }
      if(thisType=='winter'){
        writeOGR(lowusecorridor, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","low_use"), driver="ESRI Shapefile",overwrite=T)
        finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","low_use")]<<-lowusecorridor
      }

    }

    if(thisType=='migration'){
      writeOGR(stopovers, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","stopovers"), driver="ESRI Shapefile",overwrite=T)
      finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","stopovers")]<<-stopovers
    }
    if(thisType=='winter'){
      writeOGR(stopovers, corridorsShapefiles, paste0(groupNameForFiles,"_",thisType,"_","core"), driver="ESRI Shapefile",overwrite=T)
      finalPolygonsHolder[paste0(groupNameForFiles,"_",thisType,"_","core")]<<-stopovers
    }



    progressIndicator('reclassifying corridors','stop')

    if(lastOneBoth==TRUE){
      ###########################
      ###### TAB COMPLETED ######
      ###########################
      finalMergingRunning<<-FALSE
      shinyjs::enable("mergeAnimalsButton")
      progressTracker$completed<<-'tab8'
      progressTracker$finalMerging<<-TRUE
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
      tab8PolliesAdded<<-TRUE
      addPolygonsToMap()

      modalMessager('Analysis Complete',"All analyses have completed. You can
      see the population-level corridors on the map below. These shapefiles
      have been written to the 'PopulationShapefiles' folder in the Project directory
      you selected. You will also find 1) shapefiles of each individual's
      migration corridors, 2) the GPS locations of each migration, 3) a comma
      delimited file of the dates of start and end of migration, and 4) raster
      files representing the utilization distribution of population-level
      corridors and each animal's migration. Once you have finished exploring
      the results on the map below, you can close the App.")
    }
  }
}
