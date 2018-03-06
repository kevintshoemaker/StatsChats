stepOneFunction<-function(input,output,session){
### ---- ###
## SERVER FUNCTION STEP 1
###### IMPORT dataset
### ---- ###
stepOneHappened<<-TRUE


observeEvent(input$getStartedButton, {
  toggleModal(session,'welcomeModal',toggle='close')
},ignoreInit=TRUE)

toggleModal(session,'welcomeModal',toggle='open')



observeEvent(input$chooseDirButton, {
  ##--------------------------------CHOOSE THE DATA FOLDER AND MAKE A LIST OF SHPS
  # dataFolder<-"C:\\Users\\Josh\\Dropbox\\collarProcessingApp\\data\\sampleData"
  # dataFolder <<-"D:\\Documents\\Dropbox\\collarProcessingApp\\data\\sampleData"
  dataFolder<<-choose.dir()
  # print(dataFolder)
  availableShapefiles <<- list.files(dataFolder, pattern = '.shp')
  if (length(availableShapefiles) == 0) {
    modalMessager(
      "Folder Selection Error",
      "No valid shapefile are present in this directory. Please check the
      directory and try again"
    )
    return
  }
  availableShapefiles <- append("", availableShapefiles)

  ##--------------------------------make a label showing selected fer
  output$selectedDirectoryLabel <- renderUI({
    p(paste("You successfully imported ", dataFolder, sep = ""))
  })

  ##--------------------------------render the dropdown for available shapes
  output$selectedShapefileHeaderLabel <- renderUI({
    strong('(2) Choose shapefile(s) from the selected data directory')
  })
  output$fileUploadSelectorHolder <- renderUI({
    selectInput(
      "fileUploadSelector",
      "",
      availableShapefiles,
      selected = NULL,
      multiple = TRUE
    )
  })

  ##------------------ start file import
  output$fileUploadExecute<-renderUI({
      actionButton('fileUploadExecute','Begin File Import')
  })
})

  ##--------------------------------Observer for shapefile selection
  fileImportTracker<<-list()
  importedShapefilesHolder<<-list()

  observeEvent(input$noMergeData,{
    toggleModal(session,'moreDataModal',toggle='toggle')
    print('no')
  })

  observeEvent(input$yesMergeData,{
    toggleModal(session,'moreDataModal',toggle='toggle')

    if(is.null(input$fileUploadSelector)){
      modalMessager('Error','You need to select a shapefile to continue import')
      return()
    }

    delay(250,importStarter(TRUE))


  })

  observeEvent(input$fileUploadExecute, {
    print('click')
    if(exists('importedDatasetMaster')){
      toggleModal(session,'moreDataModal',toggle='toggle')
      return()
    }
    if(is.null(input$fileUploadSelector)){
      modalMessager('Error','You need to select a shapefile to continue import')
      return()
    }

    importStarter(FALSE)
  })


  importStarter<-function(merging){
    filesToImport<-input$fileUploadSelector
    fileImportTracker<<-list()
    importedShapefilesHolder<<-list()

    for(i in 1:length(filesToImport)){
      fileToImport <- tools::file_path_sans_ext(filesToImport[i])
      # HANDLER TO INDICATE THIS IS THE LAST ONE
      if(i<length(filesToImport)){
        importShapefile(fileToImport,FALSE,i,merging)
      } else{
        importShapefile(fileToImport,TRUE,i,merging)
      }
    }
  }


  importShapefile<-function(fileToImport,lastOne,i,mergingFiles){
    ##--------------------------------read the shapfile and overwrite the
    ##--------------------------------global importedDataset variable
    # first check if the file has already been imported.. this weeds out multipl
    # clicks on upload button or re-uploads etc
    if(!is.null(fileImportTracker[[fileToImport]])){
      if(fileImportTracker[[fileToImport]]=="inProgress"){

        return()
      }
      if(fileImportTracker[[fileToImport]]=="failed"){

        return()
      }
      if(fileImportTracker[[fileToImport]]==TRUE){

        return()
      }
    }

    importedDataset <<- tryCatch({
      fileImportTracker[[fileToImport]]<<-"inProgress"
      progressIndicator(paste('Importing ',fileToImport,' Please wait',sep=""),'start')
      readOGR(dsn = dataFolder ,
        layer = fileToImport,
        verbose = FALSE)
    },
    error = function(cond) {
      fileImportTracker[[fileToImport]]<<-'failed'
      progressIndicator('Import Error','stop')
      modalMessager(
        "DATA IMPORT ERROR",
        paste(
          "There was a fatal error importing
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
    },
    warning = function(cond) {
      fileImportTracker[[fileToImport]]<<-'failed'
      progressIndicator('Import Error','stop')
      modalMessager("DATA IMPORT WARNING", cond)
      return()
    })
    importSuccessHandler(fileToImport,lastOne,i,mergingFiles)
  }

  clearShapefileSelector<-function(){
    updateSelectInput(session=session, "fileUploadSelector",
      label = "",
      choices = availableShapefiles,
      selected = NULL
    )
  }


  importSuccessHandler<-function(fileToImport,lastOne,i,mergingFiles){
    # keep track of position in imports --- to do.. this will probs get goofy if we delete and reimport over and over again
    importIterator<-i
    progressIndicator(paste(fileToImport,' Imported successfully!',sep=""),'stop')
    # make note that this file has been imported successfully
    fileImportTracker[[fileToImport]]<<-TRUE
    # add this shapefile to list holder of imported shapefiles
    importedShapefilesHolder[[fileToImport]]<<-assign(fileToImport,importedDataset)
    # temp ui element for reference
    tempUiElement<-paste("uploadedShapefile",importIterator,sep="")
    # create delete button
    # output[[tempUiElement]]<-renderUI({
    #   div(style="display: inline-block;vertical-align:top; width: 150px; padding:5px;",actionButton(tempUiElement,fileToImport))
    # })
    # delete handler
    # observeEvent(input[[tempUiElement]], {
    #   output[[tempUiElement]]<-renderUI({})
    #     # delete from the import tracker
    #     fileImportTracker[[fileToImport]]<<-NULL
    #     # delete from the shapefile holder
    #     importedShapefilesHolder[[fileToImport]]<<-NULL
    #     observeEvent(input[[tempUiElement]], {})
    #     # if the last shapefile was deleted, clear the id selection area
    #     if(length(importedShapefilesHolder)==0){
    #       output$uniqueIdTitle<-renderUI({})
    #       output$uniqueIdSelector<-renderUI({})
    #       output$uniqueIdSelectorGo<-renderUI({})
    #       }
    # },ignoreInit=TRUE)

    clearShapefileSelector()
    if(lastOne==TRUE){
      checkColumnsPrjs(mergingFiles)
    }
  }

  checkColumnsPrjs<-function(mergingFiles){
    #### check if columns are the same between datasets
    firstNames<-names(importedShapefilesHolder[[1]]@data)
    for(i in 1:length(importedShapefilesHolder)){
      theseNames<-names(importedShapefilesHolder[[i]]@data)
      if(!setequal(firstNames,theseNames)){
        modalMessager('COLUMN NAMES ERROR',"Column names are not the same between your datasets.
        Reformat your data and try again")
        #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
        clearAllFiles()
        return()
      }
    }


    # check that projections are the same between datasets
    for(i in 1:length(importedShapefilesHolder)){
      if(!projection(importedShapefilesHolder[[1]])==projection(importedShapefilesHolder[[i]])){
        modalMessager('PROJECTION ERROR',"Projections are not the same between your datasets.
        Reformat your data and try again")
        #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
        clearAllFiles()
        return()
      }
    }
    if(!mergingFiles){
      showWorkingDirectorySelect();
      return()
    }

    if(mergingFiles){
      showColumnChoiceInfo()
    }
  }

  clearAllFiles<-function(){
    # clearing all files
    for(i in 1:length(importedShapefilesHolder)){
      tempUiElement<-paste0("uploadedShapefile",i)
      # create delete button
      output[[tempUiElement]]<-renderUI({})
    }
    fileImportTracker<<-list()
    importedShapefilesHolder<<-list()
    output$uniqueIdTitle<-renderUI({})
    output$uniqueIdSelector<-renderUI({})
    output$uniqueIdSelectorGo<-renderUI({})
  }

  showWorkingDirectorySelect<-function(){
    output$workingDirectoryTitle<-renderUI({
      HTML('<strong>(2) Click the button below to choose and empty Project Folder where all outputs will be stored.</strong>')
    })

    output$chooseWorkingDirButton<-renderUI({
      actionButton("chooseWorkingDirButton", "Click to Choose Dirctory")
    })

    ##------------------choose a folder where all export files will be stored


    observeEvent(input$chooseWorkingDirButton, {
      masterWorkingDirectory<<-NULL
      print('nulled')
      shinyjs::disable("chooseWorkingDirButton")

      masterWorkingDirectory<<-choose.dir(dataFolder)
      print('nulled')

      files<-list.files(masterWorkingDirectory)

      if(length(files)>0){
        modalMessager('error','The folder you chose is not empty.
        This will cause errors in analysis. Please empty the folder or
        choose a different directory and try again.')
        shinyjs::enable("chooseWorkingDirButton")
        masterWorkingDirectory<<-NULL
        print('nulled')
        return()
      }



      averagedIndividualsDir <<- paste(masterWorkingDirectory,"averagedIndivids", sep="\\")
      if(dir.exists(averagedIndividualsDir)==FALSE){
        dir.create(averagedIndividualsDir)
      }
      ##------------------ create directories for all working files
      # make a directory to hold all the individual shapefiles
      individualsBbDir <<- paste(masterWorkingDirectory,"individBb", sep="\\")
      if(dir.exists(individualsBbDir)==FALSE){
        dir.create(individualsBbDir)
      }

      removedPointsDirectory <<- paste(masterWorkingDirectory,"removedPoints", sep="\\")
      if(dir.exists(removedPointsDirectory)==FALSE){
        dir.create(removedPointsDirectory)
      }

      shapefilesDir <<- paste(masterWorkingDirectory,"individShapefiles", sep="\\")
      if(dir.exists(shapefilesDir)==FALSE){
        dir.create(shapefilesDir)
      }
      shapefilesDirMig <<- paste(shapefilesDir,"migrations", sep="\\")
      if(dir.exists(shapefilesDirMig)==FALSE){
        dir.create(shapefilesDirMig)
      }
      shapefilesDirWin <<- paste(shapefilesDir,"winter", sep="\\")
      if(dir.exists(shapefilesDirWin)==FALSE){
        dir.create(shapefilesDirWin)
      }

      rasterDirectory<-paste(masterWorkingDirectory,"rasters",sep="\\")
      if(dir.exists(rasterDirectory)==FALSE){
        dir.create(rasterDirectory)
      }
      rasterDirectoryIndividuals <<- paste(masterWorkingDirectory,"rasters","individuals", sep="\\")
      if(dir.exists(rasterDirectoryIndividuals)==FALSE){
        dir.create(rasterDirectoryIndividuals)
      }
      rasterDirectoryGroups <<- paste(masterWorkingDirectory,"rasters","groups", sep="\\")
      if(dir.exists(rasterDirectoryGroups)==FALSE){
        dir.create(rasterDirectoryGroups)
      }

      corridorsShapefiles <<- paste(masterWorkingDirectory,"PopulationShapefiles", sep="\\")
      if(dir.exists(corridorsShapefiles)==FALSE){
        dir.create(corridorsShapefiles)
      }

      migtimesDirectory <<- paste(masterWorkingDirectory,"migtimes", sep="\\")
      if(dir.exists(migtimesDirectory)==FALSE){
        dir.create(migtimesDirectory)
      }

      gpsPointsDirectory <<- paste(masterWorkingDirectory,"gpsPoints", sep="\\")
      if(dir.exists(gpsPointsDirectory)==FALSE){
        dir.create(gpsPointsDirectory)
      }

      gpsPointsDirectoryMigration <<- paste(gpsPointsDirectory,"migration", sep="\\")
      if(dir.exists(gpsPointsDirectoryMigration)==FALSE){
        dir.create(gpsPointsDirectoryMigration)
      }
      gpsPointsDirectoryWinter <<- paste(gpsPointsDirectory,"winter", sep="\\")
      if(dir.exists(gpsPointsDirectoryWinter)==FALSE){
        dir.create(gpsPointsDirectoryWinter)
      }
      workingFilesDirectory <<- paste(masterWorkingDirectory,"workingFiles", sep="\\")
      if(dir.exists(workingFilesDirectory)==FALSE){
        dir.create(workingFilesDirectory)
      }
      dbbTracking <<- paste(workingFilesDirectory,"dbbTracker", sep="\\")
      if(dir.exists(dbbTracking)==FALSE){
        dir.create(dbbTracking)
      }

      output$selectedWorkingDirectoryLabel<-renderUI({
        strong(paste0('Your data will be exported to: ',masterWorkingDirectory))
      })


      output$selectedWorkingDirectoryLabel<-renderUI({
        HTML(paste0('<strong>',masterWorkingDirectory,'</strong>'))
      })

      showColumnChoiceInfo()
      shinyjs::enable("chooseWorkingDirButton")
    },ignoreInit=TRUE)

  }

  showColumnChoiceInfo<-function(){

    output$uniqueIdTitle<-renderUI({
      HTML('<p>Your dataset(s) have been successfully imported. You now need to
      identify a unique animal identifier. Using the dropdown below, select the
      column that contains a unique ID for each animal in your dataset. This
      will be mandatory if you have imported a single shapefile with merged
      individuals. If you have imported many files with multiple individuals
      and do not have a unique ID column, choose "NaN" from the dropdown, and
      a unique ID will be created from the name of each file.<br><br>
      Once you make a choice in the dropdown, press the EXECUTE button to
      continue preparing your data.</p>')
      })

    output$uniqueIdSelector<-renderUI({
      selectInput(
        "uniqueIdSelector",
        "",
        c("",'NaN',names(importedShapefilesHolder[[1]])),
        selected = NULL,
        multiple = FALSE
      )
    })
    output$uniqueIdSelectorGo<-renderUI({
      actionButton('uniqueIdSelectorGo','Execute')
    })
    observeEvent(input$uniqueIdSelectorGo, {

      if(is.null(masterWorkingDirectory)){
        modalMessager('Error','You need to select an empty working directory to continue')
        return()
      }

      mergeShapfilesHandler()
    })

  }

  mergeShapfilesHandler<-function(){

    selectedUid<-input$uniqueIdSelector

    if(selectedUid==""){
      modalMessager('Error','You need to select a field for the unique
      identifier or choose NaN if your dataset does not have a unique ID')
      return()
    }

    progressIndicator('Processing.... Please wait...','start')



    # if nan is chosen then the uid will just be the filename
    if(selectedUid=='NaN'){
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        thisNewUid<-names(importedShapefilesHolder[i])
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_", "", thisNewUid)
        importedShapefilesHolder[[i]]@data['newUid']<<-thisNewUid
      }
    } else{ #otherwise the UID is the field that was selected for the UID
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        # also cannot have underscores.. could cause errors later
        importedShapefilesHolder[[i]]@data['newUid']<<-gsub("_","",importedShapefilesHolder[[i]]@data[,selectedUid])
      }
    }

    # if data was imported and importedDatasetMaster exists then create a
    # new temp dataset to bind to the existing master
    if(exists('importedDatasetMaster')){
      mergingNewData<<-TRUE
      progressTracker$mergingNewData<<-mergingNewData
      importedDatasetOld<<-importedDatasetMaster
      progressTracker$importedDatasetOld<<-importedDatasetOld
      saveProgressTracker()
      saveRDS(importedDatasetOld, file=paste0(workingFilesDirectory,"\\", 'importedDatasetOld.rds'))


      js$disableTab("panel2")
      js$disableTab("panel3")
      js$disableTab("panel4")
      js$disableTab("panel5")
      js$disableTab("panel6")
      js$disableTab("panel7")
      js$disableTab("panel8")
      js$disableTab("panel9")
    }


    importedDatasetMaster <<- tryCatch({
        Reduce(rbind,importedShapefilesHolder)
      },
      error = function(cond) {
      modalMessager(
        "DATASET MERGE ERROR",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      },
      warning = function(cond) {
      modalMessager(
        "DATASET MERGE WARNING",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      }
    )

  progressIndicator('Processing.... Please wait...','stop')

  projectShapefilesHandler()
}

projectShapefilesHandler<-function(){

  importedDatasetMaster <<- tryCatch({
      spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    },
    error = function(cond) {
    modalMessager(
      "PROJECTION ERROR",
      paste(
        "There was a fatal error projecting
        your datasets. Please check the data and try again. Detailed error from
        R is : ",
        cond,
        sep = ""
      )
    )
    return()
    },
    warning = function(cond) {
    modalMessager(
      "PROJECTION WARNING",
      paste(
        "There was a fatal error projecting
        your datasets. Please check the data and try again. Detailed error from
        R is : ",
        cond,
        sep = ""
      )
    )
    return()
    }
  )

  # add lat/lon for leaflet maps
  importedDatasetMaster@data[["lon"]]<<-importedDatasetMaster$coords.x1
  importedDatasetMaster@data[["lat"]]<<-importedDatasetMaster$coords.x2

  importedDatasetMaster <<- tryCatch({
      # spTransform(importedDatasetMaster,CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
      spTransform(importedDatasetMaster,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    },
    error = function(cond) {
    modalMessager(
      "DATASET MERGE ERROR",
      paste0(
        "There was a fatal error merging
        your datasets. Please check the data and try again. Detailed error from
        R is : ",cond))
    return()
    },
    warning = function(cond) {
    modalMessager(
      "DATASET MERGE WARNING",
      paste(
        "There was a fatal error merging
        your datasets. Please check the data and try again. Detailed error from
        R is : ",
        cond,
        sep = ""
      )
    )
    return()
    }
  )


  ###########################
  ###### TAB COMPLETED ######
  ###########################
  progressTracker$completed<<-'tab1'

  progressTracker$idsCreated<<-TRUE

  configPageUpdated<<-TRUE

  saveProgressTracker()
  saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
  js$enableTab("panel2")
  # js$enableTab("configPanel")
  stepTwoFunction(input,output,session)
  updateTabsetPanel(session=session, "appTabPanels",
      selected = 'panel2'
  )

}

observeEvent(input$resumeProjectButton, {
  resumeProjectFunction()
  configPageUpdated<<-TRUE
})

}



# shape <- readOGR(dsn = "D:\\Documents\\Dropbox\\collarProcessingApp\\data\\sampleData", layer = "RD2H_227")
# shape <- readOGR(dsn = "D:\\454\\corridorShapes", layer = "high_use_corridor")
# shape <- readOGR(dsn = "D:\\454\\corridorShapes", layer = "stopovers")
# shape<-spTransform(shape,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
