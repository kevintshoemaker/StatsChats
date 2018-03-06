timeZones<-c("GMT","US/Alaska","US/Arizona","US/Pacific","US/Mountain","US/Central","US/Eastern","Canada/Saskatchewan")
selectedTimezone<<-"GMT"



##------------------ lookup for R tech values and literal values
dateTimesList<-list(
  '%H'='Hour (1-24)',
  '%I'='Hour (1-12)',
  '%p'='AM/PM',
  '%M'='Minute (1-60)',
  '%S'='Second (0-60)',
  '%m'='Month (number: 1-12)',
  '%d'='Day (number: 1-31)',
  '%y'='Year (two digits: 17)',
  '%Y'='Year (four digits: 2017)',
  '%mon'='Month (abbreviated: Jan)',
  '%month'='Month (full name: January)'
  )

  ##------------------ reverse lookup
  dateTimesListFlipped<-list(
    'Hour (1-24)'='%H',
    'Hour (1-12)'='%I',
    'AM/PM'='%p',
    'Minute (1-60)'='%M',
    'Second (0-60)'='%S',
    'Month (number: 1-12)'='%m',
    'Day (number: 1-31)'='%d',
    'Year (two digits: 17)'='%y',
    'Year (four digits: 2017)'='%Y'
    # 'Month (abbreviated: Jan)'='%mon',
    # 'Month (full name: January)'='%month'
    )


##match("Second (0-60)",possibledateTimeEle)
## returns 3


possibleTimeSeperators<-c(':',' ')
possibleDateSeperators<-c('/','-','\\',' ','No Seperator')

dates<-c('month','day','year')
times<-c('hour','minute','second')


# dateTimeEleShrt<- c('%H','%M','%S','%m','%d','%Y','%y','%mon','%month')
dateTimeLookup<-list(
  '%H'='hour',
  '%I'='hour',
  '%M'='minute',
  '%S'='second',
  '%m'='month',
  '%d'='day',
  '%Y'='year',
  '%y'='year',
  '%p'='am/pm'
  # '%mon'='month',
  # '%month'='month'
  )

validatorValues<<-c('year','month','day','hour','minute','second')


columnFieldList<<-list()

stepThreeFunction<-function(input,output,session){

  print('step three')

  stepThreeHappened<<-TRUE

  ampmtime<<-FALSE

  output$dateConfigTable <- renderTable(importedDatasetMaster@data[1:20,dateColumns])

  if(mergingNewData){
    progressTracker$columnFieldList<<-NULL
  }

  ##--- CLEAR ALL UI ELEMENTS IN CASE USER HAS GONE BACK AND MADE CHANGES
  lapply(1:12, function(i) {
    tempVar<-paste("dateConfigUi",i,sep="")
    output[[tempVar]] <- renderUI({})
  })



  ###-- create multiselectors for each of the selected date/time columns
  lapply(1:length(dateColumns), function(i) {
    tempVar<-paste("dateConfigUi",i,sep="")
    tempSelectorName<-paste(dateColumns[i],"Indicator",sep="")


    #if the columnFieldList exists in the progress tracker, we have to fill
    #in the appropriate selections in the selectors - furthermore, they have to be
    # in a specific order or R will rearrange elements
    if('columnFieldList'%in%names(progressTracker)){
      if(dateColumns[i]%in%names(progressTracker$columnFieldList)){
        # these are the date time elements that have already been selected for
        # this field dateColumns[i]
        selectedColumns<-progressTracker$columnFieldList[[dateColumns[i]]]
        #these are the selected key and value from the datetime list
        selectedKeys<-dateTimesList[progressTracker$columnFieldList[[dateColumns[i]]]]
        #next we need to grab those list() items from the date times flipped
        #to reconstruct the select menu in the correct order
        newSelectedColumns<-c()
        for(j in 1:length(selectedKeys)){
          newSelectedColumns<-c(newSelectedColumns,selectedKeys[[j]])
        }
        #these are the elements that need to be added to select menu first in
        #appropriate order
        includedDateTimeElements<-dateTimesListFlipped[newSelectedColumns]
        excludedDateTimeElements<-which(!names(dateTimesListFlipped) %in% newSelectedColumns)
        excludedDateTimeElements<-dateTimesListFlipped[excludedDateTimeElements]
        newOrderedChoices<-c(includedDateTimeElements,excludedDateTimeElements)

        # updateSelectInput(session, tempSelectorName, label = NULL, selected = selectedColumns,newOrderedChoices)
        print('rendering ')
        print(tempSelectorName)
        output[[tempVar]] <- renderUI({
          selectInput(
            tempSelectorName,
            paste("Choose date time elements for: ",
            dateColumns[i],sep=""),
            selected=selectedColumns,
            newOrderedChoices,
            multiple=TRUE
            )
        })

        observeEvent(input[[tempSelectorName]], {
          collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
        },ignoreInit=TRUE)
    }
  } else{
    selectedColumns<-NULL
    print('rendering ')
    print(tempSelectorName)
    output[[tempVar]] <- renderUI({
      selectInput(
        tempSelectorName,
        paste("Choose date time elements for: ",
        dateColumns[i],sep=""),
        selected=selectedColumns,
        dateTimesListFlipped,
        multiple=TRUE
        )
    })

    observeEvent(input[[tempSelectorName]], {
      collectDateTimeElements(input[[tempSelectorName]],dateColumns[i])
    },ignoreInit=TRUE)
  }



    observeEvent(input$testButton, {
      d<-c("%Y" ,"%m" ,"%d" ,"%H" ,"%M" ,"%S")
      updateSelectInput(session, tempSelectorName, label = NULL, choices = NULL, selected = d)
    },ignoreInit=TRUE)
  })
  ##---------##
  ## RENDER TWO SELECTORS FOR SEPERATOR ELEMENTS IN DATE/TIME
  ##---------##
  tempVar<-paste("dateConfigUi",length(dateColumns)+1,sep="")
  tempVarTwo<-paste("dateConfigUi",length(dateColumns)+2,sep="")

  # output$dateSeperatorSelector <- renderUI({
  #   selectInput('dateSeperatorSelector','What character seperates your date elements?',possibleDateSeperators)
  # })
  # output$timeSeperatorSelector <- renderUI({
  #   selectInput('timeSeperatorSelector', 'What character seperates your time elements?',possibleTimeSeperators)
  # })

  output$timezoneSelector<-renderUI({
      selectInput('timezoneSelector', 'What time zone is your data stored in?',timeZones,selected=progressTracker$selectedTimezone)
    })

  dtvRunning<<-FALSE
  observeEvent(input$processDatesButton,{
      if(!dtvRunning){
        dateTimeValidator()
      }
  })

  observeEvent(input$timezoneSelector,{
      selectedTimezone<<-input$timezoneSelector
      progressTracker$selectedTimezone<<-selectedTimezone
  },ignoreInit=TRUE)


  ## this fires every times there is a change to the date/time selector
  ## the values are then stored in an object where the col name is the key
  ## and the value is the order of date/time elements in that value
  collectDateTimeElements<-function(selectorValues,column){
    columnFieldList[[column]]<<-selectorValues
    progressTracker$columnFieldList<<-columnFieldList
  }

  dateTimeValidator<-function(){    
    dtvRunning<<-TRUE
    #recreate the validatorObject anytime process dates is clicked
    #in case back actions and changes
    validatorObject<<-list()
    for(i in 1:length(columnFieldList)){
      # these are the names of all fields in columnFieldList
      tempNames<-names(columnFieldList)
      tempColumn<-tempNames[i]
      #these are all the dt elements in that column
      tempColumnItems<-columnFieldList[[i]]
      #since there could be multiple DT elements in one column
      #we'll iterate through even if there is only one
      for(j in 1:length(tempColumnItems)){
        #this is the particular dt value in that column
        tempValue<-tempColumnItems[j]
        ## j ends up being the position in that column
        updateKeyObj(tempValue,j,tempColumn)
      }
      ##if its the last iteration then fire the next fxn
      if(i==length(columnFieldList)){
        # processDatesFunction()
        hasObjectsHandler()
      }
    }
  }

  updateKeyObj<-function(tempValue,j,column){
    #tempvariable for literal dt value
    tempDTelementType<-dateTimeLookup[[tempValue]]
    #check if this has already been recorded in the validator object
    # print(!is.null(validatorObject[[tempDTelementType]]))
    # send this object to the
    # if there is ampm data.. we'll store this in a seperate validatorObject and return
    if(tempValue=='%p'){
      ampmtime<<-TRUE
      validatorObjectAmPm<<-list()
      validatorObjectAmPm[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
      return()
    }
    validatorObject[[tempDTelementType]]<<-c(tempDTelementType,j,tempValue,column)
  }

  processDatesFunction<-function(){
    hasObjects<-c()
    missingObjects<-c()
    for (i in 1:length(validatorValues)) {
      if(!is.null(validatorObject[[validatorValues[i]]])){
        hasObjects<-append(hasObjects,validatorValues[i])
      } else{
        missingObjects<-append(missingObjects,validatorValues[i])
      }
      ###once loop finishes then check whether or not there are missing
      ###objects or if we're ready to process dates
      if(i==length(validatorValues)){
        ##If there are missing objects
        if(length(missingObjects)>0){
          missingObjectsHandler(missingObjects)
        } else if (length(missingObjects)==0){ ###else if there are no missing
          # check to see if the new date field has already been created
          #  this prevents multiple clicks on the button
          if(is.null(importedDatasetMaster@data$newMasterDate)){
            hasObjectsHandler(hasObjects)
          }
        }
      }
    }
  }

  missingObjectsHandler<-function(missingObjects){
      modalMessager('Warning',paste('You have not chosen a complete selection
      of variables to configure data time. You are missing values
      for: ',toString(missingObjects),sep=""))
  }

  # hasObjectsHandler<-function(hasObjects){
  hasObjectsHandler<-function(){
    progressIndicator('Processing Dates.. Please wait','start')
    importedDatasetMaster@data$newMasterDate<<--999

    # if there happens to be am/pm time.. lets quickly create a vector to hold these
    # we'll paste it back to the time later
    if(ampmtime){
      # this is the column where ampm lives
      tempDtCol<-validatorObjectAmPm[[1]][4]
      # temp vector of this column
      ampm<<-importedDatasetMaster@data[,tempDtCol]
      # remove everything in this column thats not a,m,p,A,M,P
      ampm<<-gsub("[^amp|AMP]","",ampm)
    }



    #  since the loop below runs from all possible date time elements
    # errors are thrown when not all seconds, minutes etc have been selected
    # need to run this looop based in validatorObject
    selectedDateTimeElements<-names(validatorObject)
    validatorsLength<-length(selectedDateTimeElements)
    for(j in 1:validatorsLength){
      processingInfo<-paste0(sprintf("%.1f",round((j/(validatorsLength+1)*100),1)),'% ','complete')
      progressIndicator(processingInfo,'update',j/(validatorsLength+1))
      # what is the type
      ### tempDtType<-validatorObject[[validatorValues[[j]]]][1]
      tempDtType<-selectedDateTimeElements[j]
      # if(tempDtType=='am/pm'){
      #   next
      # }
      # what position is it in this specific column?
      tempIndexOfLoc<-as.numeric(validatorObject[[tempDtType]][2])
      # lookup the posix type (?necessary?)
      # tempDtPosixType<-validatorObject[[validatorValues[[j]]]][3]
      # which column is it in
      tempDtCol<-validatorObject[[tempDtType]][4]
      # create a temp vector for this DT element named as such
      assign(selectedDateTimeElements[j],importedDatasetMaster@data[,tempDtCol])
      # remove all the non numeric characters and spaces and sub
      # with commas -- the get command grabs the above variabled named as
      # month day year etc etc
      tempDateDataObjSplt<-gsub("[^0-9.]",',',get(selectedDateTimeElements[j]))
      # split on the commas
      tempDateDataObjSplt<-strsplit(tempDateDataObjSplt,",")
      # remove the empties
      tempDateDataObjSplt<-lapply(tempDateDataObjSplt,function(x){
        x[!x ==""]
      })





      # this generates a vector of T/F checking for invalid data time positions
      # in tempDateDateObjSplt... this helps weed out strange date time elements
      # for example a merged dataset where some time elements have H,M,S and
      # other only have H,M
      # lengthList<-do.call(rbind, lapply(tempDateDataObjSplt, function(x) length(x)))
      naList<-do.call(rbind, lapply(tempDateDataObjSplt, function(x) is.na(x[tempIndexOfLoc])))

      if(any(naList)){
        firstErrorRow<-which(naList)[1]
        firstErrorRowData<-importedDatasetMaster@data[firstErrorRow,tempDtCol]
        errorMsg<-paste0('You have date time elements in your data that have a
        different number of elements than you indicated. For example, row
        ',firstErrorRow,' column ',tempDtCol,' = ',firstErrorRowData)
        modalMessager('ERROR',errorMsg)
        dtvRunning<<-FALSE
        progressIndicator('Done importing dates','stop')
        break
        return()
      }

      # assign back to the temp holder just the correct index
      # of that date time element
      assign(selectedDateTimeElements[j],sapply(tempDateDataObjSplt,"[[",tempIndexOfLoc))
      # assign(validatorValues[dd],sapply(ee,"[[",ff))
      if(j==validatorsLength){
        if(!exists('year')){
          modalMessager('ERROR','no year')
          dtvRunning<<-FALSE
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('month')){
          modalMessager('ERROR','no month')
          dtvRunning<<-FALSE
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('day')){
          modalMessager('ERROR','no day')
          dtvRunning<<-FALSE
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('hour')){
          modalMessager('ERROR','no hour')
          dtvRunning<<-FALSE
          progressIndicator('Done importing dates','stop')
          return()
        }
        if(!exists('minute')){
          minute<-"00"
        }
        if(!exists('second')){
          second<-"00"
        }

        # need to check for any decimals in the data
        checkForDec<-function(element,from){
          element<-as.numeric(element)
          isDecimal<-testInteger(element)
          if(!isDecimal){
            message<-paste0('You have decimals in one of you date elements.
            You need to remove decimals from the data and start over. Decimals were
            found in column ',from,'.')
            modalMessager('ERROR',message)
            dtvRunning<<-FALSE
            progressIndicator('Done importing dates','stop')
            return(FALSE)
          }
        }

        testInteger <- function(x){
          test <- all.equal(x, as.integer(x), check.attributes = FALSE)
          if(test == TRUE){ return(TRUE) }
          else { return(FALSE) }
        }


        checkForDec(year,'year')
        checkForDec(month,'month')
        checkForDec(day,'day')
        checkForDec(hour,'hour')
        checkForDec(minute,'minute')
        checkForDec(second,'second')





        newDate<<-paste(year,month,day,sep="-")
        newTime<<-paste(hour,minute,second,sep=":")
        if(ampmtime){
          newTime<<-paste0(newTime,' ',ampm)
          rm(ampm)
        }
        newDateTime<<-paste(newDate,newTime,sep=" ")



        combineDateElements(newDateTime)
      }
    }

  }

  combineDateElements<-function(newDateTime){
    stringFormat<-"%Y-%m-%d %H:%M:%S"

    if(ampmtime){
      stringFormat<-"%Y-%m-%d %I:%M:%S %p"
    }

    importedDatasetMaster@data$newMasterDate<<-tryCatch({
      as.POSIXct(strptime(
        newDateTime,
        format = stringFormat),
        tz =selectedTimezone
      )},
      error = function(cond) {
        modalMessager('ERROR',cond)
        return()
      },
      warning = function(cond) {
        modalMessager('Warning',cond)
        return()
      }
    )

    naDatesLength<<-nrow(importedDatasetMaster@data[is.na(as.Date(importedDatasetMaster@data$newMasterDate)),])
    progressTracker$naDates<<-NULL
    progressTracker$naDatesLength<<-naDatesLength
    if(naDatesLength==nrow(importedDatasetMaster@data)){
      modalMessager('ERROR','Your selection of date elements failed.
      Check your selection of date/time elements and try again')
      dtvRunning<<-FALSE
      progressIndicator('Done importing dates','stop')
      return()
    }
    ### if NA dates are produced
    if(naDatesLength>0){
        naDatesObservations<<-importedDatasetMaster@data[is.na(as.Date(importedDatasetMaster@data$newMasterDate)),]
        importedDatasetMaster<<-importedDatasetMaster[!is.na(as.Date(importedDatasetMaster@data$newMasterDate)),]
        progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      } else{
        progressIndicator('Done importing dates','stop')
        createUniqueIdsHanlder()
      }

  }

  createUniqueIdsHanlder<-function(){
    progressIndicator('Creating unique IDs','start')

    uniqueYears<-unique(format(as.Date(importedDatasetMaster@data[,"newMasterDate"]),"%y"))
    uniqueYearsFull<-unique(format(as.Date(importedDatasetMaster@data[,"newMasterDate"]),"%Y"))

    importedDatasetMaster@data$idAmmend<<-'-999'

    for(i in 1:length(uniqueYearsFull)){
      # thisYear<-format(as.Date(newDateTime),"%Y")
      thisYear<-uniqueYearsFull[i]
      nextYear<-as.numeric(thisYear)+1
      lastYear<-as.numeric(thisYear)-1

      thisYearShort<-uniqueYears[i]
      nextYearShort<-as.numeric(thisYearShort)+1
      lastYearShort<-as.numeric(thisYearShort)-1

      lastYearEndDate<-as.Date(paste(thisYear,"01","31",sep="-"),"%Y-%m-%d")
      thisYearStartDate<-as.Date(paste(thisYear,"02","01",sep="-"),"%Y-%m-%d")
      thisYearEndDate<-as.Date(paste(nextYear,"01","31",sep="-"),"%Y-%m-%d")


      importedDatasetMaster@data[
        as.Date(importedDatasetMaster@data$newMasterDate)>=thisYearStartDate &
        as.Date(importedDatasetMaster@data$newMasterDate)<=thisYearEndDate,"idAmmend"
      ]<<-paste0(thisYearShort,nextYearShort)

    }
    progressIndicator('Success','stop')

    checkForDuplicates(FALSE)
  }

  checkForDuplicates<-function(fromMerge){
    progressIndicator('checking for duplicate data points','start')

    #REORDER DATA AS PER BURST REQUIREMENTS
    importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]

    # get length
    beforeDups<-length(importedDatasetMaster)

    tempVector<-importedDatasetMaster@data[,c("newUid","newMasterDate")]
    notDuplicated<-!duplicated(tempVector)
    areDuplicatedObservations<-duplicated(tempVector)
    areDuplicatedObservations<-importedDatasetMaster[areDuplicatedObservations,]
    progressTracker$areDuplicatedObservations<<-NULL
    if(nrow(areDuplicatedObservations)>0){
      writeOGR(areDuplicatedObservations, removedPointsDirectory, paste0('duplicates_',nrow(areDuplicatedObservations)), driver="ESRI Shapefile",overwrite=T)
      progressTracker$areDuplicatedObservations<<-nrow(areDuplicatedObservations)
    }

    importedDatasetMaster<<-importedDatasetMaster[notDuplicated,]



    progressIndicator('checking for duplicate data points','stop')
    # timeProcessingSteps()
    if(fromMerge==TRUE){
      stepThreeDoneHandler()
      return()
    }
    timeProcessingSteps()
  }

  timeProcessingSteps<-function(){
    progressIndicator('calculating burst','start')
    #REORDER DATA AS PER BURST REQUIREMENTS
    importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]
    # run the burst script
    importedDatasetMaster@data$burst<<-creat.burst(importedDatasetMaster@data)
    progressIndicator('calculating burst','stop')


    progressIndicator('calculating date parameters','start')
    importedDatasetMaster@data$month <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%m", tz = selectedTimezone))
    importedDatasetMaster@data$sprfal <<- ifelse(importedDatasetMaster@data$month %in% 2:7,"spring","fall")
    importedDatasetMaster@data$year <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%Y", tz = selectedTimezone))
    importedDatasetMaster@data$year_bio <<- ifelse(importedDatasetMaster@data$month == 1, importedDatasetMaster@data$year-1, importedDatasetMaster@data$year)   #use this bio year so animals can start/end migration fall migration in january
    importedDatasetMaster@data$jul <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%j", tz = selectedTimezone))
    importedDatasetMaster@data$jul2<<- ifelse(importedDatasetMaster@data$month == 1, importedDatasetMaster@data$jul+max(importedDatasetMaster@data$jul), importedDatasetMaster@data$jul)
    importedDatasetMaster@data$id_yr_sprfal <<- paste(importedDatasetMaster@data$newUid, importedDatasetMaster@data$year_bio, importedDatasetMaster@data$sprfal, sep="_")
    importedDatasetMaster@data$id_yr <<- paste(importedDatasetMaster@data$newUid, importedDatasetMaster@data$year_bio, sep="_")
    progressIndicator('calculating date parameters','stop')

    progressIndicator('adding x y columns','start')
    importedDatasetMaster@data$x<<-importedDatasetMaster@coords[,1]
    importedDatasetMaster@data$y<<-importedDatasetMaster@coords[,2]
    progressIndicator('adding x y columns','stop')

    progressIndicator('finding problem points','start')
    importedDatasetMaster@data$problems<<-find.problem.pts(importedDatasetMaster@data)
    progressIndicator('finding problem points','stop')

    if(any(importedDatasetMaster@data$problems)){
      print('overspeed')
      tempVector<-importedDatasetMaster@data[,c("newUid","newMasterDate","problems")]
      overSpeed<<-tempVector[tempVector$problems==TRUE,]
      writeOGR(importedDatasetMaster[c(which(importedDatasetMaster@data$problems==TRUE)),], removedPointsDirectory, paste0('overSpeed_',nrow(overSpeed)), driver="ESRI Shapefile",overwrite=T)
      importedDatasetMaster<<-importedDatasetMaster[-c(which(importedDatasetMaster@data$problems==TRUE)),]
      progressTracker$overSpeed<<-overSpeed
      saveProgressTracker()

      progressIndicator('calculating burst #2','start')
      #REORDER DATA AS PER BURST REQUIREMENTS
      importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]
      # run the burst script
      importedDatasetMaster@data$burst<<-creat.burst(importedDatasetMaster@data)
      progressIndicator('calculating burst #2','stop')

      saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
    }

    progressIndicator('calculating movement parameters','start')
    importedDatasetMaster@data<<-mov.param(importedDatasetMaster@data)
    saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
    progressIndicator('calculating movement parameters','stop')


    mortalities<<-mort.check(importedDatasetMaster@data)
    progressTracker$mortalities<<-mortalities
    totalMortalityPoints<<-0
    progressTracker$totalMortalityPoints<<-totalMortalityPoints
    if(!is.null(mortalities)){
      progressIndicator('check for mortalities in dataset','start')
      for(i in 1:nrow(mortalities)){
        thisIndivid<-mortalities[i,'newUid']
        thisStart<-mortalities[i,'date_start']
        thisEnd<-mortalities[i,'date_end']
        # creates a boolean for which rows are within each mort period
        theseMorts<- importedDatasetMaster$newUid==thisIndivid &
          importedDatasetMaster$newMasterDate>=thisStart &
          importedDatasetMaster$newMasterDate<=thisEnd
        # adds to the total mortality points counter
        totalMortalityPoints<<-totalMortalityPoints+
          length(theseMorts[theseMorts==TRUE])
        # creates a new subset of data, removing mort points
        writeOGR(importedDatasetMaster[theseMorts,], removedPointsDirectory, paste0('mortalities_',thisIndivid,'_',length(theseMorts[theseMorts==TRUE])), driver="ESRI Shapefile",overwrite=T)
        importedDatasetMaster<<-importedDatasetMaster[!theseMorts,]
      }
      progressIndicator('check for mortalities in dataset','stop')

      progressIndicator('calculating burst #3','start')


      #REORDER DATA AS PER BURST REQUIREMENTS
      importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]
      # run the burst script
      importedDatasetMaster@data$burst<<-creat.burst(importedDatasetMaster@data)
      progressIndicator('calculating burst #3','stop')

      progressIndicator('calculating movement parameters #2','start')
      importedDatasetMaster@data<<-mov.param(importedDatasetMaster@data)
      progressIndicator('calculating movement parameters #2','stop')

      saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
    }
    progressTracker$totalMortalityPoints<<-totalMortalityPoints
    buildMigtime()
  }

  buildMigtime<-function(){

    if(exists('migtime')){
      print("migtime exists")
      migtimeOld<<-migtime
      progressTracker$migtimeOld<<-migtimeOld
      saveProgressTracker()
      saveRDS(migtimeOld, file=paste0(workingFilesDirectory,"\\", 'migtimeOld.rds'))
    }



    progressIndicator('Building Migtime Table and calculating NSD','start')

    migtime <<- importedDatasetMaster@data[duplicated(importedDatasetMaster@data$id_yr)==FALSE,c("id_yr","newUid","year_bio")]
    migtime <<- migtime[order(migtime$newUid, migtime$year_bio),]
    migtime$startSpring <<- as.Date(NA)
    migtime$endSpring <<- as.Date(NA)
    migtime$startFall <<- as.Date(NA)
    migtime$endFall <<- as.Date(NA)
    migtime$springRun<<-FALSE
    migtime$fallRun<<-FALSE

    for(i in 1:nrow(migtime)){
      temp<- importedDatasetMaster@data[importedDatasetMaster@data$id_yr==migtime$id_yr[i],]
      temp$nsd <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2 #someday i'd like to fix this properly
      temp$displacement <- sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2) #someday i'd like to fix this properly
      importedDatasetMaster@data[importedDatasetMaster@data$id_yr==migtime$id_yr[i],"nsd"]<<-temp$nsd
      importedDatasetMaster@data[importedDatasetMaster@data$id_yr==migtime$id_yr[i],"displacement"]<<-temp$displacement
    }

    # if migtimeOld was created but new data is not being merged then this is running
    # because dates were being rerun.. we can still use the old migtime table but have
    # to only select those rows that are in the newest version.. this is in case
    # rows were removed because of mortalities etc
    if(exists('migtimeOld') & !mergingNewData){
      migtime<<-migtimeOld[migtimeOld$id_yr==migtime$id_yr,]
      # also, we will need to force a rerun of BB if its already been run
      rerunAllIndividualsHandler()
    }


    progressTracker$migtime<<-migtime
    progressIndicator('Building Migtime Table and calculating NSD','stop')

    if(mergingNewData){
      mergeDatasets()
      # mergeDatasets()
      return()
    }

    stepThreeDoneHandler()
  }

  mergeDatasets<-function(){
    # this creates a vector of the matching columns between the two
    # this will allow a RBIND.. we'll check for duplicates after
    sameColumns<-intersect(names(importedDatasetOld),names(importedDatasetMaster))
    importedDatasetMaster<-importedDatasetMaster[,sameColumns]
    importedDatasetOld<-importedDatasetOld[,sameColumns]
    importedDatasetMaster<<-rbind(importedDatasetMaster,importedDatasetOld)


    if(exists('migtimeOld')){
      migtimeNew<<-migtime
      migtimeOlder<<-migtimeOld
      migtime<<-rbind(migtimeOld,migtime)
    }


    if(any(duplicated(migtime[,c('newUid','year_bio')]))){
      migtime<<-migtime[!duplicated(migtime[,c('newUid','year_bio')]),]
      print('duplicate migtimes')
    }

    checkForDuplicates(TRUE)
  }


  stepThreeDoneHandler<-function(){
    # if(exists('migtimeOld')){
    #   print('merging')
    #   return()
    #   return()
    # }
    ###########################
    ###### TAB COMPLETED ######
    ###########################
    progressTracker$completed<<-'tab3'
    dtvRunning<<-FALSE
    saveRDS(importedDatasetMaster, file=paste0(workingFilesDirectory,"\\", 'importedDatasetMaster.rds'))
    js$enableTab("panel4")

    progressTracker$datesCalculated<<-TRUE

    # remove the imported shapefiles holder
    rm(importedShapefilesHolder,envir=globalenv())
    progressIndicator('saving progress','start')
    saveProgressTracker()
    progressIndicator('saving progress','stop')

    stepFourFunction(input,output,session)
    updateTabsetPanel(session=session, "appTabPanels",
        selected = 'panel4'
    )
  }
}

# simple function to convert older migtables to newer ones
migtimeFixer<-function(toFix){
  toFix$springRun<-TRUE
  toFix$fallRun<-TRUE
  toFix[toFix=='1111-11-11']<-NA
  toFix[is.na(toFix$startSpring),'springRun']<-NA
  toFix[is.na(toFix$endSpring),'springRun']<-NA
  toFix[is.na(toFix$startFall),'fallRun']<-NA
  toFix[is.na(toFix$endFall),'fallRun']<-NA
  return(toFix)
}
