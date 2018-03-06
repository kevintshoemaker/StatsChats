stepFourFunction<-function(input,output,session){
  stepFourHappened<<-TRUE
  # areDuplicated
  # naDatesObservations
  totalIndividuals<-unique(importedDatasetMaster@data$newUid)
  # if(length(totalIndividuals)<10){
  #   modalMessager('Warning',paste('You only have imported ',length(totalIndividuals),
  #   ' individuals. To build a population based corridor model
  #   it is recomended that you use at least 10 individuals.'))
  # }

  output$totalIndividualsText <- renderUI({
    strong(paste0('You imported ',length(totalIndividuals), 'unique
    individuals.'))
  })
  output$totalIndividuals <- renderUI({
    p(paste0('These individuals were named ',(paste(totalIndividuals,collapse=', '))))
  })

  dateMinimum<-min(importedDatasetMaster@data$newMasterDate)
  dateMaximum<-max(importedDatasetMaster@data$newMasterDate)




  output$datesSummaryText <- renderUI({
    strong(paste0('Your dates had a minimum value of ',dateMinimum,'
    and a maximum value of ',dateMaximum))
  })
  if(!is.null(progressTracker$naDates)){
    naDateTotal<-progressTracker$naDatesLength
    if(naDateTotal>0){
      output$datesSummary <- renderUI({
        p(paste0('There were ',naDateTotal,' dates that were NA
        in your dataset. These dates were removed'))
      })
    }
  }

  if(!is.null(progressTracker$areDuplicatedObservations)){
    output$duplicatedPointsSummaryText <- renderUI({
      strong('Duplicate Points')
    })
    output$duplicatedPointsSummary <- renderUI({
      p(paste0('There were ',progressTracker$areDuplicatedObservations,' duplicate id/time
      observation in your dataset. These observations were removed
      from the analysis. In order to bring these points back, you would need to start over with this project.'))
    })
  }

  output$mortalitiesSummaryText <- renderUI({
    p(' ')
  })
  output$mortalitiesSummary <- renderUI({
    p(' ')
  })

  output$mortalitiesTable<-renderTable(NULL)

  if(exists('mortalities')){
    if(!is.null(mortalities)){
      output$mortalitiesSummaryText <- renderUI({
        strong('Mortalities in dataset')
      })
      output$mortalitiesSummary <- renderUI({
        p(paste0('There were ',nrow(progressTracker$mortalities),' mortality
        periods in your dataset representing ',totalMortalityPoints,'
         gps locations. These points were removed from analysis.  In order to bring these points back, you would need to start over with this project.'))
      })
      mortTab<-mortalities
      mortTab$date_start<-format(mortTab$date_start,'%Y-%m-%d %H:%M:%S')
      mortTab$date_end<-format(mortTab$date_end,'%Y-%m-%d %H:%M:%S')
      output$mortalitiesTable<-renderTable(mortTab)
    }
  }

  progressIndicator('checking fix rates','start')
  #You can add this part anywhere prior to conducting BB analyses...
  temp <- importedDatasetMaster@data[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),c('newMasterDate','newUid')]  #order the database
  dif <- c(as.numeric(diff(as.numeric(temp$newMasterDate))),0)/3600  #change in time between points (in hrs)
  #add a burst column for when there are breaks larger than 8 hours in the data
  temp$fixCheck <- ifelse(dif >selectedMaxFixInterval, "bad", "good")
  totalBad<-length(which(temp$fixCheck=='bad'))
  totalGood<-length(which(temp$fixCheck=='good'))
  totalLength<-nrow(temp)
  totalBadPercent<-(totalBad/totalLength)*100

  if(nrow(temp[temp$fixCheck =="bad",])>0){
    message<-paste0('You have individuals with gaps > ',selectedMaxFixInterval ,'
    hours in the data. Given your specification of max gap in time (see Parameters tab),
    this represents ',signif(totalBadPercent,3),'% of all the data you have imported. If
    those poinst are during the migration period or during winter, they will not be
    connected in the BB analysis.')
    modalMessager('Warning',message)
  }
  rm(temp)
  progressIndicator('checking fix rates','stop')

  progressIndicator('checking fix rates 2','start')
  #You can add this part anywhere prior to conducting BB analyses...
  temp <- importedDatasetMaster@data[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),c('newMasterDate','newUid')]  #order the database
  dif <- c(as.numeric(diff(as.numeric(temp$newMasterDate))),0)/60  #change in time between points change to minutes)

  temp$fixCheck <- ifelse(dif < 2, "bad", "good")
  totalBad<-length(which(temp$fixCheck=='bad'))
  totalGood<-length(which(temp$fixCheck=='good'))
  totalLength<-nrow(temp)
  totalBadPercent<-(totalBad/totalLength)*100

  if(signif(totalBadPercent,3)>1){
    message<-paste0('You have individuals with fix rates less than 2 minutes
    hours in the data. This represents ',signif(totalBadPercent,3),'% of all the
    data you have imported. If this is a significant percentage, serious errors
    may occur in the Brownian Bridge analysis')
    modalMessager('Warning',message)
  }
  rm(temp)
  progressIndicator('checking fix rates 2','stop')

  # if(any(importedDatasetMaster@data$problems){
  #   print('overspeed')
  #   tempVector<-importedDatasetMaster@data[,c("newUid","newMasterDate","speed","problems")]
  #   overSpeed<-tempVector[tempVector$problems==TRUE,]
  #   importedDatasetMaster<<-importedDatasetMaster[-c(which(overSpeed==TRUE)),]
  #   saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
  #   output$speedPointsSummaryText <- renderUI({
  #     strong('Points over the max speed limit')
  #   })
  #   output$speedPointsSummary <- renderUI({
  #     p(paste0('There were ',length(which(overSpeed)==TRUE),' points that exceeded the speed limit
  #     of ',((maxSpeedParameter*60)*60)/1000,' km/hr. These poinst have been removed from
  #     the analysis. The max speed paramater can be adjusted in the configuration
  #     page'))
  #   })
  #   output$speedPointsTable<-renderTable(importedDatasetMaster@data[which(overSpeed==TRUE),])
  # }


  if(exists('overSpeed')){
    output$speedPointsSummaryText <- renderUI({
      strong('Points over the max speed limit')
    })
    output$speedPointsSummary <- renderUI({
      p(paste0('There were ',nrow(overSpeed) ,' points that exceeded the speed limit
      of ',((maxSpeedParameter*60)*60)/1000,' km/hr. These poinst have been removed from
      the analysis. The max speed paramater can be adjusted in the configuration
      page. You can see the first 50 points that exceeded this limit in the table below. In order to bring these points back, you would need to start over with this project.'))
    })

    toTable<-overSpeed

    if(nrow(overSpeed)>50){
      toTable<-overSpeed[1:50,]
    }
    toTable$newMasterDate<-format(toTable$newMasterDate,'%Y-%m-%d %H:%m:%S')
    output$speedPointsTable<-renderTable(toTable)
  }

  ###########################
  ###### TAB COMPLETED ######
  ###########################
  progressTracker$completed<<-'tab4'
  # js$disableTab("panel1")
  # js$disableTab("panel2")
  # js$disableTab("panel3")
  js$enableTab("panel5")
  js$enableTab("panel6")
  stepFiveFunction(input,output,session)
  stepSixFunction(input,output,session)
}

#
# uiOutput('totalIndividualsText'),
# uiOutput('totalIndividuals'),
# uiOutput('datesSummaryText'),
# uiOutput('datesSummary'),
# uiOutput('naDatesSummaryText'),
# uiOutput('naDatesSummary'),
# uiOutput('duplicatedPointsSummaryText'),
# uiOutput('duplicatedPointsSummary'),
# uiOutput('problemPointsSummaryText'),
# uiOutput('problemtPointsText')
