stepTwoFunction<-function(input,output,session){
  ### ---- ###
  ## START STEP TWO FUNCTIONS -- DEALING WITH DATES AND MULTIPLE INDIVIDUALS
  ### ---- ###
  stepTwoHappened<<-TRUE

  ##------------------ show the first 20 rows of data
  output$dateConfigTable1 <- renderTable(importedDatasetMaster@data[1:20,])

  uniqueColumns<-names(importedDatasetMaster@data)


  selectedDateColumns<-NULL
  # if there are already some data columns from a previous selection and
  # new data is not being added
  if(!is.null(progressTracker[['dateColumns']]) & !mergingNewData){
    selectedDateColumns<-progressTracker$dateColumns
  }

  updateCheckboxGroupInput(session,
    "dateColumnSelector",
      choices=uniqueColumns,
      selected=selectedDateColumns,
      inline=TRUE
    )

  if(mergingNewData){
    return()
  }




  ##------------------ add to date columns whenever one is selected
  observeEvent(input$dateColumnSelector, {
    print('date column select change')
    dateColumns <<- input$dateColumnSelector
    progressTracker$dateColumns<<-dateColumns
    # stepThreeFunction(input,output,session)
  })

  ##------------------once date columns have been selected move on to next tab
  observeEvent(input$doneChoosingDateColumnsButton,{
    print('click')
    ###########################
    ###### TAB COMPLETED ######
    ###########################
    progressTracker$completed<<-'tab2'
    js$enableTab("panel3")
    stepThreeFunction(input,output,session)

    progressTracker$dateFieldsChosen<<-TRUE

    # modalMessager("Done Selecting Dates","Double check the selected columns
    # and click the STEP 3 tab to further configure these data")
    updateTabsetPanel(session=session, "appTabPanels",
        selected = 'panel3'
    )
  })
### ---- ###
## END STEP TWO FUNCTIONS
### ---- ###
}
