configPageFunction<-function(input,output,session){

  numberOfCores <<- tryCatch({
    detectCores()[1]
  },
  error = function(cond) {
    modalMessager(
      "Core Detection Error",
      paste(
        "We were unable to determine how many cores were available in your
        processor. The app will default to 2. You can try changing this
        value in the configuration panel: ",
        cond,
        sep = ""
      )
    )
    return()
  },
  warning = function(cond) {
    progressIndicator('Import Error','stop')
    modalMessager(
      "Core Detection Error",
      paste(
        "We were unable to determine how many cores were available in your
        processor. The app will default to 2. You can try changing this
        value in the configuration panel: ",
        cond,
        sep = ""
      )
    )
    return()
  })


  if(!exists("numberOfCores")){
    numberOfCores<<-2
  }

  output$cpusText <- renderUI({
    p(paste0('This represents the number of CPU cores that Migration Mapper will use
    to run the Brownian Bridge Movement Analysis. The larger the number, the quicker
    the analysis will go. That being said, every computer has a limit, which will be
    determined by Migration Mapper and printed in the text of this parameter. We
    recommend using 1-2 less than the total number of available CPU cores on your
    computer. Upon testing, your machine has ',numberOfCores,' available.'))
  })


  updateSelectInput(session=session, "cpuSelector",
    label = "",
    choices = 1:numberOfCores,
    selected = numberOfCoresToUse
  )

  observeEvent(input$cpuSelector,{
      progressTracker$numberOfCoresToUse<<-input$cpuSelector
      numberOfCoresToUse<<-input$cpuSelector
  },ignoreInit=TRUE)


  observeEvent(input$maxSpeedSelector,{
      progressTracker$maxSpeedParameter<<-(input$maxSpeedSelector*1000)/3600
      maxSpeedParameter<<-(input$maxSpeedSelector*1000)/3600
      if(exists('migtime') & configPageUpdated){
        modalMessager('alert','Since you have already processed dates, you will need to go back to tab 3 and rerun the data processing
        by clicking the PROCESS DATES button.')
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)

  observeEvent(input$mortDistance,{
      progressTracker$mortDistance<<-input$mortDistance
      # saveProgressTracker()
      if(exists('migtime') & configPageUpdated){
        modalMessager('alert','Since you have already processed dates, you will need to go back to tab 3 and rerun the data processing
        by clicking the PROCESS DATES button.')
      }
  },ignoreInit=TRUE)

  observeEvent(input$mortTime,{
      progressTracker$mortTime<<-input$mortTime
      mortTime<<-input$mortTime
      if(exists('migtime') & configPageUpdated){
        modalMessager('alert','Since you have already processed dates, you will need to go back to tab 3 and rerun the data processing
        by clicking the PROCESS DATES button.')
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)


  observeEvent(input$rasterResolutionSelector,{
      rasterResolution<<-input$rasterResolutionSelector
      progressTracker$rasterResolution<<-rasterResolution
      print(activeTab)
      if(!is.null(activeTab)){
        if(activeTab=='configPanel'){
          rerunAllIndividualsHandler()
        }
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)

  observeEvent(input$maxFixIntervalSelector,{
      progressTracker$selectedMaxFixInterval<<-input$maxFixIntervalSelector
      selectedMaxFixInterval<<-input$maxFixIntervalSelector
      if(!is.null(activeTab)){
        if(activeTab=='configPanel'){
          rerunAllIndividualsHandler()
        }
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)

  observeEvent(input$classifyPercentileSelector,{
      progressTracker$classificationPercentile<<-input$classifyPercentileSelector
      classificationPercentile<<-input$classifyPercentileSelector
      if(!is.null(activeTab)){
        if(activeTab=='configPanel'){
          rerunAllIndividualsHandler()
        }
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)


  observeEvent(input$bbMarginSelector,{
      progressTracker$bbMargin<<-input$bbMarginSelector
      bbMargin<<-input$bbMarginSelector
      if(!is.null(activeTab)){
        if(activeTab=='configPanel'){
          rerunAllIndividualsHandler()
        }
      }
      # saveProgressTracker()
  },ignoreInit=TRUE)

  observeEvent(input$locationErrorSelector,{
    locationError<<-input$locationErrorSelector
    progressTracker$locationError<<-locationError
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }
  },ignoreInit=TRUE)

  observeEvent(input$windowSizeSelector,{
    windowSize<<-input$windowSizeSelector
    progressTracker$windowSize<<-windowSize
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }
  },ignoreInit=TRUE)

  observeEvent(input$dbbRadioSelector,{
      dbbMethod<<-input$dbbRadioSelector
      progressTracker$dbbMethod<<-dbbMethod
      if(!is.null(activeTab)){
        if(activeTab=='configPanel'){
          rerunAllIndividualsHandler()
        }
      }
  },ignoreInit=TRUE)

  observeEvent(input$nsdToPlotSelector,{
      if(input$nsdToPlotSelector=='TRUE'){
        nsdToPlot<<-'nsd'
        nsdLabel<<-'NSD (km)'
      }
      if(input$nsdToPlotSelector=='FALSE'){
        nsdToPlot<<-'displacement'
        nsdLabel<<-'Displacement (kilometers)'
      }
      progressTracker$nsdToPlot<<-nsdToPlot
      progressTracker$nsdLabel<<-nsdLabel
      # saveProgressTracker()
  },ignoreInit=TRUE)


  shinyjs::disable("winterStartEndRadio")


  observeEvent(input$winterRangeYesNoRadio,{
    calculateWinterRange<<-input$winterRangeYesNoRadio
    progressTracker$calculateWinterRange<<-calculateWinterRange
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }

    if(calculateWinterRange){
      shinyjs::enable("winterStartEndRadio")
      js$enableTab("panel8")
    } else{
      shinyjs::disable("winterStartEndRadio")
      js$disableTab("panel8")
    }
  },ignoreInit=TRUE)



  shinyjs::disable("winterStartSelector")
  shinyjs::disable("winterEndSelector")

  observeEvent(input$winterStartSelector,{
    selectedWinterStart<<-input$winterStartSelector
    progressTracker$selectedWinterStart<<-selectedWinterStart
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }
  },ignoreInit=TRUE)

  observeEvent(input$winterEndSelector,{
    selectedWinterEnd<<-input$winterEndSelector
    progressTracker$selectedWinterEnd<<-selectedWinterEnd
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }
  },ignoreInit=TRUE)

  observeEvent(input$winterStartEndRadio,{
    winterDateHandler<<-input$winterStartEndRadio
    progressTracker$winterDateHandler<<-winterDateHandler
    if(!is.null(activeTab)){
      if(activeTab=='configPanel'){
        rerunAllIndividualsHandler()
      }
    }


    if(winterDateHandler=='chosen'){
      shinyjs::enable("winterStartSelector")
      shinyjs::enable("winterEndSelector")
    } else{
      shinyjs::disable("winterStartSelector")
      shinyjs::disable("winterEndSelector")
    }



  },ignoreInit=TRUE)

  updateMeanDates<<-function(){
    if(!exists('migtime')){
      return()
    }
    allStartSpringDates<-migtime[!is.na(migtime$startSpring),'startSpring']
    allEndFallDates<-migtime[!is.na(migtime$endFall),'endFall']

    if(length(allStartSpringDates)==0 | length(allEndFallDates)==0){
      return()
    }

    if(is.na(allStartSpringDates) | is.na(allEndFallDates)){
      return()
    }

    allStartSpringDates<-format(as.Date(allStartSpringDates), format = "%m-%d")
    allEndFallDates<-format(as.Date(allEndFallDates), format = "%m-%d")

    allStartSpringDates<-paste0('2017-',allStartSpringDates)
    allEndFallDates<-paste0('2017-',allEndFallDates)

    meanSpringStartDate<-mean(as.Date(allStartSpringDates))
    meanFallEndDate<-mean(as.Date(allEndFallDates))

    meanSpringStartDateLong<<-format(meanSpringStartDate, format = "%B %d")
    meanFallEndDateLong<<-format(meanFallEndDate, format = "%B %d")

    meanSpringStartDate<<-format(meanSpringStartDate, format = "%m-%d")
    meanFallEndDate<<-format(meanFallEndDate, format = "%m-%d")

    progressTracker$meanSpringStartDate<<-meanSpringStartDate
    progressTracker$meanFallEndDate<<-meanFallEndDate

    tempText<-paste0("Average start (",meanFallEndDateLong,") and end (",meanSpringStartDateLong,") dates for selected individuals.")

    output$winterMinMaxText<-renderUI({
      p(tempText)
    })


  }
}
