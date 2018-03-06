# dataFolder<-choose.dir()
# availableShapefile <- list.files(dataFolder, pattern = '.shp')
# availableShapefile <- append("", availableShapefile)

####--------------- GLOBAL VARS AND FUNCTIONS
# ls()
# rm(list = setdiff(ls(), lsf.str()))
# print('globals')
# ls()


importedDataset <- 'null'
importedDatasetData <- 'null'
availableColumns <- 'null'
progress <- 'null'
existingPolygonsLoaded<<-FALSE
polygonsHolderMigrations<<-list()
polygonsHolderWinter<<-list()
dbbMethod<<-TRUE

configPageUpdated<<-FALSE

groupNamesMembersWinter<<-list()
groupNamesMembersMigration<<-list()


tab6PolliesAdded<<-FALSE
tab7MenusCreated<<-FALSE
tab8MenusCreated<<-FALSE
tab8PolliesAdded<<-FALSE

minLatBbs<<-NA
minLonBbs<<-NA
maxLatBbs<<-NA
maxLonBbs<<-NA

minLatMig<<-NA
minLonMig<<-NA
maxLatMig<<-NA
maxLonMig<<-NA

minLatWin<<-NA
minLonWin<<-NA
maxLatWin<<-NA
maxLonWin<<-NA


stepOneHappened<<-FALSE
stepTwoHappened<<-FALSE
stepThreeHappened<<-FALSE
stepFourHappened<<-FALSE
stepFiveHappened<<-FALSE
stepSixHappened<<-FALSE
stepSevenHappened<<-FALSE
stepEightHappened<<-FALSE
stepNineHappened<<-FALSE


progressTracker<<-list()
progressTracker$currentTab<<-NULL

activeTab<<-NULL
mergingNewData<<-FALSE
progressTracker$mergingNewData<<-mergingNewData
progressTracker$idsCreated<<-FALSE
progressTracker$dateFieldsChosen<<-FALSE
progressTracker$datesCalculated<<-FALSE
progressTracker$dateRangesSelected<<-FALSE
progressTracker$bbCompleted<<-FALSE
progressTracker$groupsChosen<<-FALSE
progressTracker$finalMerging<<-FALSE

bbMargin<<-3
classificationPercentile<<-0.99
rasterResolution<<-150
mortTime<<-48
mortDistance<<-50
selectedMaxFixInterval <<- 8
maxSpeedParameter<<-3
numberOfCoresToUse<<-2
nsdToPlot<<-'nsd'
nsdLabel<<-'NSD (km)'
winterDateHandler<<-'mean'
selectedWinterEnd<<-'2017-03-01'
selectedWinterStart<<-'2017-11-01'
calculateWinterRange<<-FALSE
totalMortalityPoints<<-0
locationError<<-20
windowSize<<-31


progressTracker$bbMargin<<-bbMargin
progressTracker$classificationPercentile<<-classificationPercentile
progressTracker$rasterResolution<<-rasterResolution
progressTracker$mortTime<<-mortTime
progressTracker$mortDistance<<-mortDistance
progressTracker$selectedMaxFixInterval <<- selectedMaxFixInterval
progressTracker$maxSpeedParameter<<-maxSpeedParameter
progressTracker$numberOfCoresToUse<<-numberOfCoresToUse
progressTracker$nsdToPlot<<-nsdToPlot
progressTracker$nsdLabel<<-nsdLabel
progressTracker$winterDateHandler<<-winterDateHandler
progressTracker$selectedWinterEnd<<-selectedWinterEnd
progressTracker$selectedWinterStart<<-selectedWinterStart
progressTracker$calculateWinterRange<<-calculateWinterRange
progressTracker$totalMortalityPoints<<-totalMortalityPoints
progressTracker$locationError<<-locationError
progressTracker$windowSize<<-windowSize

allDirectoryNames<<-c('averagedIndivids','individBb','removedPoints','individShapefiles',
'rasters','PopulationShapefiles','migtimes','gpsPoints','workingFiles')

jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

shinyjs.mapTest = function(name){
  return(3)
}



"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}
"

globalInitFunctions<-function(input, output, session){
  resumeProjectFunction<<-function(){
    # modalMessager(
    #   "Choose existing project folder",
    #   "Please select the folder that contains your existing project"
    # )
    existingProjectFolder<<-NULL
    existingProjectFolder<<-choose.dir()
    existingDirectories<-list.dirs(path = existingProjectFolder, full.names = FALSE, recursive= FALSE)
    print(existingDirectories)
    masterWorkingDirectory<<-existingProjectFolder
    if(!setequal(existingDirectories,allDirectoryNames)){
      modalMessager(
        "Choose existing project folder",
        paste0("The directories in your selected folder do not match the required
        directories. Your selected folder should contain the directories ",paste(allDirectoryNames,collapse=', '),
        '. Your selected folder contains the directories ',paste(existingDirectories,collapse=', '),'. Please
        double check the folder and try again.')
      )
      return()
    }

    averagedIndividualsDir <<- paste(masterWorkingDirectory,"averagedIndivids", sep="\\")

    individualsBbDir <<- paste(masterWorkingDirectory,"individBb", sep="\\")

    removedPointsDirectory <<- paste(masterWorkingDirectory,"removedPoints", sep="\\")
    if(dir.exists(removedPointsDirectory)==FALSE){
      dir.create(removedPointsDirectory)
    }

    shapefilesDir <<- paste(masterWorkingDirectory,"individShapefiles", sep="\\")
    shapefilesDirMig <<- paste(shapefilesDir,"migrations", sep="\\")
    shapefilesDirWin <<- paste(shapefilesDir,"winter", sep="\\")

    rasterDirectory <<- paste(masterWorkingDirectory,"rasters", sep="\\")
    rasterDirectoryIndividuals <<- paste(masterWorkingDirectory,"rasters","individuals", sep="\\")
    rasterDirectoryGroups <<- paste(masterWorkingDirectory,"rasters","groups", sep="\\")


    corridorsShapefiles <<- paste(masterWorkingDirectory,"PopulationShapefiles", sep="\\")
    migtimesDirectory <<- paste(masterWorkingDirectory,"migtimes", sep="\\")

    gpsPointsDirectory <<- paste(masterWorkingDirectory,"gpsPoints", sep="\\")
    gpsPointsDirectoryMigration <<- paste(gpsPointsDirectory,"migration", sep="\\")
    gpsPointsDirectoryWinter <<- paste(gpsPointsDirectory,"winter", sep="\\")


    workingFilesDirectory <<- paste(masterWorkingDirectory,"workingFiles", sep="\\")


    importedDatasetMaster<<-readRDS(paste(masterWorkingDirectory, 'workingFiles','importedDatasetMaster.rds', sep="\\"))
    progressTracker<<-readRDS(paste(masterWorkingDirectory, 'workingFiles','progressFile.rds', sep="\\"))

    updateNumericInput(session, 'maxSpeedSelector', value = (progressTracker$maxSpeedParameter*3600)/1000)
    updateNumericInput(session, 'maxFixIntervalSelector', value = progressTracker$selectedMaxFixInterval)
    updateNumericInput(session, 'mortDistance', value = progressTracker$mortDistance)
    updateNumericInput(session, 'mortTime', value = progressTracker$mortTime)
    updateNumericInput(session, 'rasterResolutionSelector', value = progressTracker$rasterResolution)
    updateNumericInput(session, 'classifyPercentileSelector', value = progressTracker$classificationPercentile)
    updateNumericInput(session, 'bbMarginSelector', value = progressTracker$bbMargin)
    updateSelectInput(session, 'cpuSelector', selected = progressTracker$numberOfCoresToUse)
    updateSelectInput(session, 'timezoneSelector', selected = progressTracker$selectedTimezone)

    updateNumericInput(session, 'windowSizeSelector', value = progressTracker$windowSize)
    updateNumericInput(session, 'locationErrorSelector', value = progressTracker$locationError)




    if("windowSize" %in% names(progressTracker)){
      windowSize<<-progressTracker$windowSize
    }

    if("overSpeed" %in% names(progressTracker)){
      overSpeed<<-progressTracker$overSpeed
    }

    if("locationError" %in% names(progressTracker)){
      locationError<<-progressTracker$locationError
    }

    if("mortalities" %in% names(progressTracker)){
      mortalities<<-progressTracker$mortalities
    }

    if("totalMortalityPoints" %in% names(progressTracker)){
      totalMortalityPoints<<-progressTracker$totalMortalityPoints
    }

    if("calculateWinterRange" %in% names(progressTracker)){
      calculateWinterRange<<-progressTracker$calculateWinterRange
      updateRadioButtons(session, "winterRangeYesNoRadio",selected = calculateWinterRange)
      if(calculateWinterRange==FALSE){
        shinyjs::disable("winterStartEndRadio")
        js$disableTab("panel8")
      }
      if(calculateWinterRange){
        js$enableTab("panel8")
      }
    }


    if("selectedWinterEnd" %in% names(progressTracker)){
      selectedWinterEnd<<-progressTracker$selectedWinterEnd
      updateDateInput(session, 'winterEndSelector', value = selectedWinterEnd)
    }

    if("selectedWinterStart" %in% names(progressTracker)){
      selectedWinterStart<<-progressTracker$selectedWinterStart
      updateDateInput(session, 'winterStartSelector', value = selectedWinterStart)
    }

    if("migtime" %in% names(progressTracker)){
      migtime<<-progressTracker$migtime
      updateMeanDates()
    }

    if("dbbMethod" %in% names(progressTracker)){
      dbbMethod<<-progressTracker$dbbMethod
      updateRadioButtons(session, "dbbRadioSelector",selected = dbbMethod)
    }

    if("winterDateHandler" %in% names(progressTracker)){
      winterDateHandler<<-progressTracker$winterDateHandler
      updateRadioButtons(session, "winterStartEndRadio",selected = winterDateHandler)
    }

   if("nsdToPlot" %in% names(progressTracker)){
      nsdToPlot<<-progressTracker$nsdToPlot
      if(nsdToPlot=='nsd'){
        tempSelect<-TRUE
      }
      if(nsdToPlot=='displacement'){
        tempSelect<-FALSE
      }
      updateRadioButtons(session, "nsdToPlotSelector",selected = tempSelect)
    }



    if(progressTracker$completed=="tab1"){
      js$enableTab("panel2")
      # js$enableTab("configPanel")
      stepTwoFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel2'
      )
    }
    if(progressTracker$completed=="tab2"){
      js$enableTab("panel2")
      js$enableTab("panel3")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel3'
      )
    }
    if(progressTracker$completed=="tab3"){
      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel4'
      )
    }
    if(progressTracker$completed=="tab4"){

      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      js$enableTab("panel5")
      js$enableTab("panel6")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel5'
      )
    }
    if(progressTracker$completed=="tab5"){

      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      js$enableTab("panel5")
      js$enableTab("panel6")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      stepSixFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel6'
      )
    }
    if(progressTracker$completed=="tab6"){

      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      js$enableTab("panel5")
      js$enableTab("panel6")
      js$enableTab("panel7")
      if(calculateWinterRange){
        js$enableTab("panel8")
      }
      js$enableTab("panel9")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      stepSixFunction(input,output,session)
      stepSevenFunction(input,output,session)
      stepEightFunction(input,output,session)
      stepNineFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel7'
      )
    }
    if(progressTracker$completed=="tab7"){

      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      js$enableTab("panel5")
      js$enableTab("panel6")
      js$enableTab("panel7")
      if(calculateWinterRange){
        js$enableTab("panel8")
      }
      js$enableTab("panel9")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      stepSixFunction(input,output,session)
      stepSevenFunction(input,output,session)
      stepEightFunction(input,output,session)
      stepNineFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel7'
      )
    }
    if(progressTracker$completed=="tab8"){
      js$enableTab("panel2")
      js$enableTab("panel3")
      js$enableTab("panel4")
      js$enableTab("panel5")
      js$enableTab("panel6")
      js$enableTab("panel7")
      if(calculateWinterRange){
        js$enableTab("panel8")
      }
      js$enableTab("panel9")
      # js$enableTab("configPanel")
      dateColumns<<-progressTracker$dateColumns
      stepTwoFunction(input,output,session)
      stepThreeFunction(input,output,session)
      stepFourFunction(input,output,session)
      stepSixFunction(input,output,session)
      stepSevenFunction(input,output,session)
      stepEightFunction(input,output,session)
      stepNineFunction(input,output,session)
      updateTabsetPanel(session=session, "appTabPanels",
          selected = 'panel7'
      )
    }
  }

  progressIndicator<<-function(message,startStop,updateValue){
    if(startStop=="start")  {
      progress <<- shiny::Progress$new()
      progress$set(message = message, value = 50)
    }
    else if(startStop=="update"){
      progress$set(message = message, value = updateValue)
    } else {
      progress$set(message = message, value = 100)
      progress$close()
    }
  }

  modalMessager <<- function(header, body) {
    showModal(modalDialog(title = header, body,footer=modalButton("OK")))
  }

  observeEvent(input$appTabPanels,{
    activeTab<<-input$appTabPanels
    print(activeTab)
    progressTracker$currentTab<<-activeTab

    if(exists('workingFilesDirectory')){
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
    }


    if(activeTab=='panel7'){
      if(!stepSevenHappened){
        return()
      }
      if(length(polygonsHolderMigrations)>0){
        leafletProxy("groupsMap") %>%
          fitBounds(minLonMig,minLatMig,maxLonMig,maxLatMig)
        delay(500,mapIndividualsGroupPageMigration())
        delay(500,createSelectMenusMigrations())
        delay(500,addExistingGroupsMigrations())
      }
    }

    if(activeTab=='panel8'){
      if(!stepEightHappened){
        return()
      }
      if(length(polygonsHolderWinter)>0){
        leafletProxy("groupsMapWinter") %>%
          fitBounds(minLonWin,minLatWin,maxLonWin,maxLatWin)
        delay(500,mapIndividualsGroupPageWinter())
        delay(500,createSelectMenusWinter())
        delay(500,addExistingGroupsWinter())
      }
    }


  },ignoreInit=TRUE,ignoreNULL=TRUE)

  saveProgressTracker<<-function(){
    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
  }
}

allTabs<-c('panel1','panel2','panel3','panel4','panel5',
'panel6','panel7','panel8','panel9','configPanel')

lockTabs<<-function(){

}

rerunAllIndividualsHandler<<-function(){
  if(exists('migtime')){
    migtime[migtime==FALSE]<<-TRUE
    progressTracker$migtime<<-migtime
    thisCompleted<-progressTracker$completed
    if(thisCompleted=='tab9' | thisCompleted=='tab8' | thisCompleted=='tab7' | thisCompleted=='tab6'){
      progressTracker$completed<<-'tab5'
    }

    js$disableTab("panel7")
    js$disableTab("panel8")
    js$disableTab("panel9")
    saveProgressTracker()
  }
}
