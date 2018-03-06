stepFiveFunction<-function(input,output,session){

  print('step five')

  winterColor<<-'#647f81'
  summerColor<<-'#959133'
  springColor<<-'#00f0ff'
  fallColor<<-'#a51cbb'

  currentIndividual<<-NULL

  julianColors<-colorRampPalette(c("green", "red", "blue"))(400)

  if(mergingNewData){
    saveProgressTracker()
    selectOptions<-list()
    currentIndividual<<-1
    totalAnimalYears<<-nrow(migtime)
    importedDatasetMaster@data$color<<-julianColors[importedDatasetMaster@data$jul2]
    importedDatasetMaster@data$springFallMarker<<-'none';
    for(i in 1:nrow(migtime)){
      thisMigName<-toString(migtime[i,"id_yr"])
      selectOptions[[thisMigName]]=toString(i)
    }
    updateSelectInput(session=session, "currentIndividualSelector",
      label = paste0("Current Animal/Year (",currentIndividual,' of ',nrow(migtime),')'),
      choices = selectOptions,
      selected = currentIndividual
    )

    mergingNewData<<-FALSE
    progressTracker$mergingNewData<<-mergingNewData
  }

  if(stepFiveHappened){
    print('step five exit')
    return()
  }
  print('step five first time')
  stepFiveHappened<<-TRUE
  # clean up the datasets
  # importedDatasetMaster <<- importedDatasetMaster[,c("x","y","id","id_yr","lat","lon","date","month","year","year_bio","jul","jul2","speed","nsd")]

  if(!exists('migtime')){
    migtime<<-progressTracker$migtime
  }

  currentIndividual<-1
  totalAnimalYears<<-nrow(migtime)
  selectOptions<-list()
  for(i in 1:nrow(migtime)){
    thisMigName<-toString(migtime[i,"id_yr"])
    selectOptions[[thisMigName]]=toString(i)
  }

  springSliderDisabled<-FALSE
  fallSliderDisabled<-FALSE

  julianColors<-colorRampPalette(c("green", "red", "blue"))(400)
  importedDatasetMaster@data$color<<-julianColors[importedDatasetMaster@data$jul2]
  importedDatasetMaster@data$springFallMarker<<-'none';



  # updateSelectInput(session,currentIndividualSelector)



  updateSelectInput(session=session, "currentIndividualSelector",
    label = paste0("Current Animal/Year (",currentIndividual,' of ',nrow(migtime),')'),
    choices = selectOptions,
    selected = currentIndividual
  )

  selectUpdater<-function(name){
    updateSelectInput(session=session, "currentIndividualSelector",
      label = paste0("Current Animal/Year (",currentIndividual,' of ',nrow(migtime),')'),
      # choices = selectOptions,
      selected = currentIndividual
    )
    # sort( sapply(ls(),function(x){object.size(get(x))}))
    # switch back to the annual NSD panel after changing individuals
    updateTabsetPanel(session=session, "graphPanels",
        selected = 'graph1tab'
    )
  }

  updateSlider<-function(data){

   thisYear<-format(min(as.Date(data$newMasterDate)),format="%Y")
   minDate<-as.Date(paste0(thisYear,"-02","-01"),"%Y-%m-%d")
   maxDate<-as.Date(paste0(as.numeric(thisYear)+1,"-01","-31"),"%Y-%m-%d")


    if(is.na(migtime[currentIndividual,"startSpring"])){
      curMinSpring<-minDate
    } else{
      curMinSpring<-as.Date(migtime[currentIndividual,"startSpring"],"%Y-%m-%d")
    }

    if(is.na(migtime[currentIndividual,"endSpring"])){
      curMaxSpring<-maxDate
    } else{
      curMaxSpring<-as.Date(migtime[currentIndividual,"endSpring"],"%Y-%m-%d")
    }

    if(is.na(migtime[currentIndividual,"startFall"])){
      curMinFall<-minDate
    } else{
      curMinFall<-as.Date(migtime[currentIndividual,"startFall"],"%Y-%m-%d")
    }

    if(is.na(migtime[currentIndividual,"endFall"])){
      curMaxFall<-maxDate
    } else{
      curMaxFall<-as.Date(migtime[currentIndividual,"endFall"],"%Y-%m-%d")
    }

    updateSliderInput(session, "dateSliderSpring",
    value = c(curMinSpring,curMaxSpring),
    min = minDate,
    max = maxDate
    )

    updateSliderInput(session, "dateSliderFall",
    value = c(curMinFall,curMaxFall),
    min = minDate,
    max = maxDate
    )

    springSliderUpdated<<-TRUE
    fallSliderUpdated<<-TRUE
  }

  observeEvent(input$skipDateSelect,{
    chosenFile <<- tryCatch({
        file.choose()
      },
      error = function(cond) {
        chosenFile<<-NULL
      }
    )

    if(!is.null(chosenFile)){
      migtimesImportHandler(chosenFile)
    }
  })

  checkForFinishedDates<<-function(){
    missingDates<-c()
    for(i in 1:nrow(migtime)){
      # if(is.na(migtime[i,"startSpring"]) | is.na(migtime[i,"endSpring"]) | is.na(migtime[i,"startFall"]) | is.na(migtime[i,"endFall"])){
      if(!is.na(migtime[i,"springRun"]) & !is.na(migtime[i,"fallRun"])){
        if(is.na(migtime[i,"startSpring"]) | is.na(migtime[i,"endSpring"]) | is.na(migtime[i,"startFall"]) | is.na(migtime[i,"endFall"])){
          missingDates<-c(missingDates,migtime[i,"id_yr"])
        }
      } else if(is.na(migtime[i,"springRun"]) & !is.na(migtime[i,"fallRun"])){
        if(is.na(migtime[i,"startFall"]) | is.na(migtime[i,"endFall"])){
          missingDates<-c(missingDates,migtime[i,"id_yr"])
        }
      } else if(!is.na(migtime[i,"springRun"]) & is.na(migtime[i,"fallRun"])){
        if(is.na(migtime[i,"startSpring"]) | is.na(migtime[i,"endSpring"])){
          missingDates<-c(missingDates,migtime[i,"id_yr"])
        }
      } else {
        missingDates<-c(missingDates,NA)
      }
    }
    if(length(missingDates[!is.na(missingDates)])>0){
      return(returnObj<-list("finished"=FALSE,"missingDates"=missingDates))
    } else if (length(missingDates[!is.na(missingDates)])==0){
      return(returnObj<-list("finished"=TRUE,"missingDates"=missingDates))
    }
  }

  # isFinished<-checkForFinishedDates()
  # if(isFinished$finished==FALSE){
  #   return()
  # }
  # if(isFinished$finished){
  #   stepSixFunction(input,output,session)
  #   updateTabsetPanel(session=session, "appTabPanels",
  #       selected = 'panel6'
  #   )
  # }



  doneSelectingDatesHandler<-function(){
    print('doneSelectingDatesHandler')
    isFinished<-checkForFinishedDates()
    progressTracker$isFinished<<-isFinished
    if(isFinished$finished==FALSE){
      modalMessager('Error',paste0('You have not yet selected dates or
      skipped animals in the dataset. You are missing dates for the
      following animals/seasons ',toString(isFinished$missingDates[!is.na(isFinished$missingDates)])))
    } else if (isFinished$finished==TRUE){
      ###########################
      ###### TAB COMPLETED ######
      ###########################
      modalMessager('Finished','That was the final animal/season in your dataset.
      If you are satisfied with the migration dates you have identified, proceed to
      tab 6-BROWNIAN BRIDGE')

      progressTracker$completed<<-'tab5'


      progressTracker$dateRangesSelected<<-TRUE
      stepSixFunction(input,output,session)
    }
   }

  migtimesImportHandler<-function(chosenFile){
    if(tools::file_ext(chosenFile)!='rds'){
      modalMessager('Error','To resume a date selection process,
      you can only import the rds file from the migtimes folder
      in your working directory. Please chose the rds file and try again')
      return
    }
    if(tools::file_ext(chosenFile)=='rds'){
      migtime<<-readRDS(chosenFile)
        for(i in 1:nrow(migtime)){
          # when resuming find which row has na first.. this is where we should
          # pick back up
          # complete cases will return false if a row has na values
          # if(!complete.cases(migtime[i,])){
          if(!is.na(migtime[i,'springRun']) & !migtime[i,'springRun']){
            currentIndividual<<-i
            selectUpdater()
            return()
          }
          if(!is.na(migtime[i,'fallRun']) & !migtime[i,'fallRun']){
            currentIndividual<<-i
            selectUpdater()
            return()
          }
          if(i==nrow(migtime)){
            currentIndividual<<-i
            selectUpdater()
            doneSelectingDatesHandler()
          }
        }
        # js$disableTab("panel4")
        # stepSixFunction(input,output,session)
        # updateTabsetPanel(session=session, "appTabPanels",
        #     selected = 'panel6'
        # )
    }
  }

  if(file.exists(paste0(migtimesDirectory,'\\','migtimes.rds'))){
      migtimesImportHandler(paste0(migtimesDirectory,'\\','migtimes.rds'))
  }

  springSliderUpdated<<-FALSE
  fallSliderUpdated<<-FALSE

  observeEvent(input$dateSliderSpring,{
    if(springSliderUpdated==TRUE){
      springSliderUpdated<<-FALSE
      return()
    }
    sliderValue<-input$dateSliderSpring
    startDate<-as.Date(sliderValue[1],"%Y-%m-%d")
    endDate<-as.Date(sliderValue[2],"%Y-%m-%d")
    dawg<<-migtime
    ci<<-currentIndividual
    migtime[currentIndividual,"startSpring"]<<-startDate
    migtime[currentIndividual,"endSpring"]<<-endDate
    migtime[currentIndividual,"springRun"]<<-TRUE
    progressTracker$migtime<<-migtime
    updateMeanDates()
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
  },ignoreInit=TRUE,ignoreNULL=TRUE)


  observeEvent(input$dateSliderFall,{
    if(fallSliderUpdated==TRUE){
      fallSliderUpdated<<-FALSE
      return()
    }
    sliderValue<-input$dateSliderFall
    startDate<-as.Date(sliderValue[1],"%Y-%m-%d")
    endDate<-as.Date(sliderValue[2],"%Y-%m-%d")
    migtime[currentIndividual,"startFall"]<<-startDate
    migtime[currentIndividual,"endFall"]<<-endDate
    migtime[currentIndividual,"fallRun"]<<-TRUE
    progressTracker$migtime<<-migtime
    updateMeanDates()
    saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
    saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
  },ignoreInit=TRUE,ignoreNULL=TRUE)



  updateSpringRadios<-function(){
    # if(!is.na(migtime[currentIndividual,"startSpring"]) & migtime[currentIndividual,"startSpring"]==("1111-11-11")){
      if(is.na(migtime[currentIndividual,"springRun"])){
        updateRadioButtons(session, "migrationPresentRadioSpring",selected = FALSE)
        if(!springSliderDisabled){
          shinyjs::disable("dateSliderSpring")
          springSliderDisabled<<-TRUE
        }
        return()
    }
    updateRadioButtons(session, "migrationPresentRadioSpring",selected = TRUE)
    if(springSliderDisabled){
      shinyjs::enable("dateSliderSpring")
      springSliderDisabled<<-FALSE
    }
  }

  updateFallRadios<-function(){
    # if(!is.na(migtime[currentIndividual,"startFall"]) & migtime[currentIndividual,"startFall"]==("1111-11-11")){
    if(is.na(migtime[currentIndividual,"fallRun"])){
        updateRadioButtons(session, "migrationPresentRadioFall",selected = FALSE)
        if(!fallSliderDisabled){
          shinyjs::disable("dateSliderFall")
          fallSliderDisabled<<-TRUE
        }
        return()
    }
    updateRadioButtons(session, "migrationPresentRadioFall",selected = TRUE)
    if(fallSliderDisabled){
      shinyjs::enable("dateSliderFall")
      fallSliderDisabled<<-FALSE
    }
  }

  observeEvent(input$migrationPresentRadioSpring,{
    if(input$migrationPresentRadioSpring==FALSE){
      # migtime[currentIndividual,"startSpring"]<<-"1111-11-11"
      # migtime[currentIndividual,"endSpring"]<<-"1111-11-11"
      migtime[currentIndividual,"springRun"]<<-NA
      print('na spring at 263')
      print(migtime[currentIndividual,'id_yr'])
      if(!springSliderDisabled){
        shinyjs::disable("dateSliderSpring")
        springSliderDisabled<<-TRUE
      }
      return()
    }
    if(springSliderDisabled){
      print('true spring at 269')
      print(migtime[currentIndividual,'id_yr'])
      migtime[currentIndividual,"springRun"]<<-TRUE
      shinyjs::enable("dateSliderSpring")
      springSliderDisabled<<-FALSE
    }
  },ignoreInit=TRUE)

  observeEvent(input$migrationPresentRadioFall,{
    if(input$migrationPresentRadioFall==FALSE){
      # migtime[currentIndividual,"startFall"]<<-"1111-11-11"
      # migtime[currentIndividual,"endFall"]<<-"1111-11-11"
      migtime[currentIndividual,"fallRun"]<<-NA
      print('na fall at 285')
      print(migtime[currentIndividual,'id_yr'])
      progressTracker$migtime<<-migtime
      saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
      if(!fallSliderDisabled){
        shinyjs::disable("dateSliderFall")
        fallSliderDisabled<<-TRUE
      }
      return()
    }
    if(fallSliderDisabled){
      print('true fall at 295')
      print(migtime[currentIndividual,'id_yr'])
      migtime[currentIndividual,"fallRun"]<<-TRUE
      saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
      progressTracker$migtime<<-migtime
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
      shinyjs::enable("dateSliderFall")
      fallSliderDisabled<<-FALSE
    }
  },ignoreInit=TRUE)


  dataForPlots<-reactive({
    tempIndex<-as.numeric(input$currentIndividualSelector)
    if('displacement' %in% names(importedDatasetMaster)){
      tempD<-importedDatasetMaster@data[
        importedDatasetMaster@data$id_yr == migtime[tempIndex,"id_yr"],
        c('newMasterDate','speed','color','nsd','displacement')
      ]
    } else{
      tempD<-importedDatasetMaster@data[
        importedDatasetMaster@data$id_yr == migtime[tempIndex,"id_yr"],
        c('newMasterDate','speed','color','nsd')
      ]
    }

    return(tempD)
  })

  dataForMap<-reactive({
    fallMin<-as.Date(fallBoxMin())
    fallMax<-as.Date(fallBoxMax())
    springMin<-as.Date(springBoxMin())
    springMax<-as.Date(springBoxMax())
    tempIndex<-as.numeric(input$currentIndividualSelector)
    tempD<-importedDatasetMaster@data[
      importedDatasetMaster@data$id_yr == migtime[tempIndex,"id_yr"],
    ]
    tempD<-tempD[!duplicated(tempD$jul2),c('jul2','lon','lat','newMasterDate')]
    # tempD$seasonColor<-'#b700ed'
    tempD$seasonColor<-winterColor
    # winter
    tempD[as.Date(tempD$newMasterDate)<springMin |
    as.Date(tempD$newMasterDate)>fallMax,'seasonColor']<-winterColor

    if(format(springMin,"%m%d")=="0201" & format(fallMax,"%m%d")=="0201"){
      tempD[,'seasonColor']<-'#626262'
    }
    # summer
    tempD[as.Date(tempD$newMasterDate)>springMax &
    as.Date(tempD$newMasterDate)<fallMin,'seasonColor']<-summerColor
    #springMig
    tempD[as.Date(tempD$newMasterDate)>=springMin &
    as.Date(tempD$newMasterDate)<=springMax,'seasonColor']<-springColor
    #fallMig
    tempD[as.Date(tempD$newMasterDate)>=fallMin &
    as.Date(tempD$newMasterDate)<=fallMax,'seasonColor']<-fallColor

    return(tempD)
  })

  springNsdData<-reactive({
    springMin<-as.Date(springBoxMin()-14)
    springMax<-as.Date(springBoxMax()+14)
    tempD<-dataForPlots()
    tempD<-tempD[as.Date(tempD$newMasterDate)>=springMin & as.Date(tempD$newMasterDate)<=springMax,]
    return(tempD)
  })

  fallNsdData<-reactive({
    fallMin<-as.Date(fallBoxMin()-14)
    fallMax<-as.Date(fallBoxMax()+14)
    tempD<-dataForPlots()
    tempD<-tempD[as.Date(tempD$newMasterDate)>=fallMin & as.Date(tempD$newMasterDate)<=fallMax,]
    return(tempD)
  })

  yearBio<-reactive({
    tempIndex<-as.numeric(input$currentIndividualSelector)
    tempYear<-paste0(migtime[tempIndex,"year_bio"],'-02-01')
    print(tempYear)
    return(tempYear)
  })

  yearBioNext<-reactive({
    tempIndex<-as.numeric(input$currentIndividualSelector)
    tempYear<-paste0(as.numeric(migtime[tempIndex,"year_bio"])+1,'-01-31')
    print(tempYear)
    return(tempYear)
  })


  springBoxMin<-reactive({
    tempData<-as.Date(input$dateSliderSpring[1])
    return(tempData)
    # as.numeric(format(as.Date(input$dateSliderSpring[1],"%Y-%m-%d"),"%j"))
  })

  springBoxMax<-reactive({
    tempData<-as.Date(input$dateSliderSpring[2])
    if(format(tempData,"%m%d")=="0131" & format(springBoxMin(),"%m%d")=="0201"){
      return(springBoxMin())
    } else{
      return(tempData)
    }
    # as.numeric(format(as.Date(input$dateSliderSpring[2],"%Y-%m-%d"),"%j"))
    })

  fallBoxMin<-reactive({
    tempData<-as.Date(input$dateSliderFall[1])
    return(tempData)
    # as.numeric(format(as.Date(input$dateSliderFall[1],"%Y-%m-%d"),"%j"))
    })

  fallBoxMax<-reactive({
    tempData<-as.Date(input$dateSliderFall[2])
    # minDate<-as.Date(paste0(thisYear,"-02","-01"),"%Y-%m-%d")
    # maxDate<-as.Date(paste0(as.numeric(thisYear)+1,"-01","-31"),"%Y-%m-%d")
    if(format(tempData,"%m%d")=="0131" & format(fallBoxMin(),"%m%d")=="0201"){
      return(fallBoxMin())
    } else{
      return(tempData)
    }
    # as.numeric(format(as.Date(input$dateSliderFall[2],"%Y-%m-%d"),"%j"))
    })


  output$plot1 <- renderPlot({
    nsdPlot<-ggplot(dataForPlots(), aes(as.Date(newMasterDate), get(nsdToPlot)/1000)) +
      annotate("rect", xmin=springBoxMin(), xmax=springBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=springColor)+
      annotate("rect", xmin=fallBoxMin(), xmax=fallBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=fallColor)+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 0.75, color=dataForPlots()$color)+
      xlab('Date')+
      ylab(nsdLabel)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
      scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
      # stat_smooth(size=1, method = "loess", level = 0.95, fullrange = TRUE, se = TRUE)
    print(nsdPlot)
  })

  output$plot2 <- renderPlot({
    springNsdPlot<-ggplot(springNsdData(), aes(as.Date(newMasterDate), get(nsdToPlot)/1000)) +
      annotate("rect", xmin=springBoxMin(), xmax=springBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=springColor)+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 0.75, color=springNsdData()$color)+
      xlab('Date')+
      ylab(nsdLabel)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
      # scale_x_date(limits = as.Date(c(springBoxMin()-14,springBoxMax()+14)))
      # stat_smooth(size=1, method = "loess", level = 0.95, fullrange = TRUE, se = TRUE)
    print(springNsdPlot)
  })

  output$plot3 <- renderPlot({
    fallNsdPlot<-ggplot(fallNsdData(), aes(as.Date(newMasterDate), get(nsdToPlot)/1000)) +
      annotate("rect", xmin=fallBoxMin(), xmax=fallBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=fallColor)+
      geom_line(size= 0.5, color="black")+
      geom_point(size = 0.75, color=fallNsdData()$color)+
      xlab('Date')+
      ylab(nsdLabel)+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
      # scale_x_date(limits = as.Date(c(fallBoxMin()-14,fallBoxMax()+14)))
      # stat_smooth(size=1, method = "loess", level = 0.95, fullrange = TRUE, se = TRUE)
    print(fallNsdPlot)
  })

  # output$plot4 <- renderPlot({
  #   speedPlot<-ggplot(dataForPlots(), aes(as.Date(newMasterDate), speed)) +
  #     annotate("rect", xmin=springBoxMin(), xmax=springBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=springColor)+
  #     annotate("rect", xmin=fallBoxMin(), xmax=fallBoxMax(), ymin=0, ymax=Inf,alpha=0.65, fill=fallColor)+
  #     geom_line(size= 0.25, color="grey")+
  #     geom_point(size = 0.75,color=dataForPlots()$color)+
  #     xlab('Date')+
  #     scale_x_date(limits = as.Date(c(yearBio(),yearBioNext())))
  #     # geom_vline(xintercept=boxMin())
  #     # stat_smooth(size=1, method = "loess", level = 0.95, fullrange = TRUE, se = TRUE)
  #     # scale_y_continuous(trans=log_trans())
  #   print(speedPlot)
  # })



 output$pointsMap <- renderLeaflet({
    print('renderLeaflet observer')
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16))%>%

    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

    addTiles(group = "OSM (default)") %>%
    addLayersControl(
      baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
      options = layersControlOptions(collapsed = TRUE)
      )%>%
    setView(lng = -111, lat = 35,zoom = 2)#%>%
  })


  mappedIndividual<<--999

  observeEvent(input$pointsMap_zoom,{
    observe({
      tempMapData<-dataForMap()
      foobar<<-tempMapData
      print('map observer')
      leafletProxy("pointsMap") %>%
        clearMarkers()%>%
        clearShapes()%>%
        addPolylines(
          weight=1.5,
          color='#422539',
          opacity=1,
          lng=tempMapData$lon,
          lat=tempMapData$lat
        )

      winterPoints<-tempMapData[tempMapData$seasonColor==winterColor,]
      springPoints<-tempMapData[tempMapData$seasonColor==springColor,]
      summerPoints<-tempMapData[tempMapData$seasonColor==summerColor,]
      fallPoints<-tempMapData[tempMapData$seasonColor==fallColor,]

      tempMapData<-rbind(winterPoints,summerPoints,springPoints,fallPoints)

      leafletProxy("pointsMap") %>%
        addCircleMarkers(
          radius=4,
          fillOpacity=0.65,
          # fillColor=julianColors[dataForMap()$jul2],
          fillColor=tempMapData$seasonColor,
          stroke=FALSE,
          popup=tempMapData$newMasterDate,
          # stroke=ifelse(dataForMap()$springFallMarker=='none',FALSE,TRUE),
          # strokeColor=ifelse(dataForMap()$springFallMarker=='spring','#3ed401','#d401bf'),
          tempMapData$lon,
          tempMapData$lat,
          tempMapData$newMasterDate
        ) #%>%
        # fitBounds(
        #   min(dataForMap()$lon),
        #   min(dataForMap()$lat),
        #   max(dataForMap()$lon),
        #   max(dataForMap()$lat)
        #   )

        if(mappedIndividual!=currentIndividual){
          leafletProxy("pointsMap") %>%
            fitBounds(
              min(dataForMap()$lon),
              min(dataForMap()$lat),
              max(dataForMap()$lon),
              max(dataForMap()$lat)
              )
        }
        mappedIndividual<<-currentIndividual
     })
   },ignoreInit=TRUE,once=TRUE)








  data<-importedDatasetMaster@data[
    importedDatasetMaster@data$id_yr == migtime[currentIndividual,"id_yr"] #&
    #importedDatasetMaster@data$year_bio == migtime[currentIndividual,"year_bio"]
    # importedDatasetMaster@data$sprfal == migtime[currentIndividual,"sprfal"]
    ,c('newUid','newMasterDate')
  ]

  # updateSlider(data)
  # updateSpringRadios()
  # updateFallRadios()
  # selectUpdater()

  toggleAnimal<-function(){
    print('animal toggle')
    print(currentIndividual)
    data<-importedDatasetMaster@data[
      importedDatasetMaster@data$id_yr == migtime[currentIndividual,"id_yr"] #&
      #importedDatasetMaster@data$year_bio == migtime[currentIndividual,"year_bio"] &
      # importedDatasetMaster@data$sprfal == migtime[currentIndividual,"sprfal"]
      ,c('newUid','newMasterDate')
    ]
    updateSlider(data)
    updateSpringRadios()
    updateFallRadios()
    selectUpdater()
    }
    toggleAnimal()


  observeEvent(input$currentIndividualSelector,{
    changedValue<-as.numeric(input$currentIndividualSelector)
    currentIndividual<<-changedValue
    toggleAnimal()
  },ignoreInit=TRUE)



  observeEvent(input$backwardHandlerButton,{
    if(currentIndividual>1){
      currentIndividual<<-currentIndividual-1
      saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
      progressTracker$migtime<<-migtime
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
      selectUpdater()
    }
  })

  observeEvent(input$forwardHandlerButton,{
    if(currentIndividual<totalAnimalYears){
      currentIndividual<<-currentIndividual+1
      saveRDS(migtime, file=paste0(migtimesDirectory,"\\", 'migtimes.rds'))
      progressTracker$migtime<<-migtime
      saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
      selectUpdater()
      return()
    }
    if(currentIndividual==totalAnimalYears){
      doneSelectingDatesHandler()
    }
  })
}
