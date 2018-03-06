stepSevenFunction<-function(input,output,session){

  if('groupNamesMembersMigration' %in% names(progressTracker)){
    groupNamesMembersMigration<<-progressTracker$groupNamesMembersMigration
  }


  if(stepSevenHappened){
    return()
  }

  stepSevenHappened<<-TRUE
  existingPolygonsMappedTab7<<-FALSE

  groupsObjectMigration<<-c()

  # migTimeTable<-read.csv(file.choose())
  groupNumber<-0

  # groupColors<-c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
  groupColors<-c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

  updateSelectMenus<-function(){
    if(length(polygonsHolderMigrations)==0){
      return()
    }
    for(i in 1:length(polygonsHolderMigrations)){
      thisIndividual<-names(polygonsHolderMigrations[i])
      whichSelected<-input[[paste0(thisIndividual,'groupMigration')]]
      if(is.null(whichSelected)){
        return()
      }
      # if this one is already selected we might have to change the selection
      # as the name is updated
      if(nchar(whichSelected)>0){
        # this is the position in the groups object from the names/members
        thisGroupNumber<-as.numeric(groupNamesMembersMigration[[thisIndividual]][1])
        # and the name value for resetting the select
        whichSelected<-groupsObjectMigration[thisGroupNumber]
      }
      updateSelectInput(session=session, paste0(thisIndividual,'groupMigration'),
        choices = groupsObjectMigration,
        selected = whichSelected
      )
    }

  }

  addGroupFunctionMigration<-function(groupName,groupNumber){
    if(is.null(groupName) & is.null(groupNumber)){
      # if there are already groups
      if(length(groupsObjectMigration)>0){
        # make sure the last one has a name
        if(groupsObjectMigration[length(groupsObjectMigration)]==""){
          modalMessager('','Make sure to choose a name for the group you created')
          return()
        }
      }
      thisGroupNumber<-length(groupsObjectMigration)+1
      tempInputValue<-''
      groupsObjectMigration[thisGroupNumber]<<-""
      # if the user tries to add another group without CHOOSING
      # a name for the group that's already been added

    }
    if(!is.null(groupName)){
      tempInputValue<-groupName
    }
    if(!is.null(groupNumber)){
      thisGroupNumber<-groupNumber
    }
    # add a UI text input for the groups name
    # removeUI(
    #   selector = paste0("groupLabelMigration",thisGroupNumber)
    # )

    removeUI(selector = paste0(".shiny-input-container:has(>#",paste0("groupLabelMigration",thisGroupNumber),")"))
    # removeUI(selector = paste0('.shiny-input-container:has(#%s)',paste0("groupLabelMigration",thisGroupNumber)))

    insertUI(
      selector = "#addGroupButtonMigration",
      where = "afterEnd",
      ui = textInput(
        inputId=paste0("groupLabelMigration",thisGroupNumber),
        label=paste("Choose Name for Group",thisGroupNumber),
        value=tempInputValue
        )
    )

    # this observer watches changes to the group names
    # if there is a change then all the select menu values are updated
    observeEvent(input[[paste0("groupLabelMigration",thisGroupNumber)]], {
    # observe({
      # tempGroupName<-thisInputValue()
      tempGroupName<-input[[paste0("groupLabelMigration",thisGroupNumber)]]

      if(!is.null(tempGroupName)){
        if(nchar(tempGroupName)>0){
          groupsObjectMigration[thisGroupNumber]<<-c(thisGroupNumber=tempGroupName)
          progressTracker$groupsObjectMigration<<-groupsObjectMigration
          updateSelectMenus()
        }
      }
    },ignoreInit=TRUE)
  }

  addExistingGroupsMigrations<<-function(){
    if('groupsObjectMigration' %in% names(progressTracker)){
      groupsObjectMigration<<-progressTracker$groupsObjectMigration
      for(i in 1:length(groupsObjectMigration)){
        addGroupFunctionMigration(groupsObjectMigration[i],i)
      }
      progressTracker$completed<<-'tab7'
    }
  }
  addExistingGroupsMigrations()


  #attach observers to select menus
  selectObserver<-function(thisIndividual){

    print(thisIndividual)

    observeEvent(input[[paste0(thisIndividual,'groupMigration')]], {
      # currently selected group
      selectedGroup<-input[[paste0(thisIndividual,'groupMigration')]]
      if(selectedGroup==""){
        if(thisIndividual %in% names(groupNamesMembersMigration)){
          groupNamesMembersMigration[[thisIndividual]]<<-NULL
          progressTracker$groupNamesMembersMigration<<-groupNamesMembersMigration
          saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
          mapIndividualsGroupPageMigration()
        }
      }
      # if(selectedGroup==""){
      #   groupNamesMembersMigration[[thisIndividual]]<<-c(99,'noGroup')
      # }
      # which position in group names is this group?
      groupPosition<-match(selectedGroup,groupsObjectMigration)
      if(!is.na(groupPosition)){

        # when resuming an existing project, when all select menus
        if(thisIndividual %in% names(groupNamesMembersMigration)){
          if(groupNamesMembersMigration[[thisIndividual]]==c(groupPosition,selectedGroup)){
            return()
          }
        }

        groupNamesMembersMigration[[thisIndividual]]<<-c(groupPosition,selectedGroup)
        progressTracker$groupNamesMembersMigration<<-groupNamesMembersMigration
        saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
        mapIndividualsGroupPageMigration()
      }
    },ignoreInit=TRUE)
  }


  # for each of the individuals, create a select menu
  createSelectMenusMigrations<<-function(){
    if(length(polygonsHolderMigrations)==0){
      return()
    }
    for(i in 1:length(polygonsHolderMigrations)){
      print('creating menu')
      print(i)
      thisIndividual<-names(polygonsHolderMigrations[i])
      selectedValue<-NULL
      # check if this individual has already been assigned a group
      if(thisIndividual %in% names(progressTracker$groupNamesMembersMigration)){
        selectedValue<-progressTracker$groupNamesMembersMigration[[thisIndividual]][2]
      }
      selectValues<-c('')
      if(length(groupsObjectMigration)>0){
        selectValues<-c('',groupsObjectMigration)
      }


      removeUI(
        selector = sprintf('.shiny-input-container:has(#%s)',paste0(thisIndividual,'groupMigration'))
      )

      insertUI(
        selector = "#animalGroupSectionMigration",
        where = "afterEnd",
        ui = selectInput(
              paste0(thisIndividual,'groupMigration'),
              thisIndividual,
              selectValues,
              selected = selectedValue,
              multiple = FALSE
          )
      )
      selectObserver(thisIndividual)
    }
    tab7MenusCreated<<-TRUE

  }
  if(!tab7MenusCreated){
    createSelectMenusMigrations()
  }






  ## to do
  ## add rows below map for each animal
  ## each time a group is added, add checkboxes or multiselect for individual

  output$groupsMapMigration <- renderLeaflet({
     leaflet(options = leafletOptions(minZoom = 6, maxZoom = 16))%>%
     addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo Base") %>%
     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
     addTiles(group = "OSM (default)") %>%
     addLayersControl(
       baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
       options = layersControlOptions(collapsed = TRUE))%>%
    setView(lng = -111,lat = 42,zoom = 6)
  })

  mapRendered<-FALSE
  observeEvent(input$groupsMapMigration_zoom,{
      if(!mapRendered){
        mapRendered<<-TRUE
        mapIndividualsGroupPageMigration()
        leafletProxy("groupsMapMigration") %>%
          fitBounds(minLonMig,minLatMig,maxLonMig,maxLatMig)
      }
  })

  mapIndividualsGroupPageMigration<<-function(){
    if(length(polygonsHolderMigrations)==0){
      return()
    }
    leafletProxy("groupsMapMigration")%>%
      clearShapes()

    for(i in 1:length(polygonsHolderMigrations)){
      tempPoly<-polygonsHolderMigrations[[i]]
      tempPolyName<-names(polygonsHolderMigrations[i])

      thisFillColor<-'#232928'
      if(tempPolyName %in% names(groupNamesMembersMigration)){
        thisFillColor<-groupNamesMembersMigration[[tempPolyName]][1]
        thisFillColor<-groupColors[as.numeric(thisFillColor)]
      }
      leafletProxy("groupsMapMigration") %>%
        addPolygons(data=tempPoly,
          fillColor=thisFillColor,
          fillOpacity=0.65,
          color='#232928',
          weight=1.5,
          popup=tempPolyName,
          group=tempPolyName
          )
    }

    leafletProxy("groupsMapMigration") %>%
      addLayersControl(
        overlayGroups = names(polygonsHolderMigrations),
        baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
  }

  observeEvent(input$addGroupButtonMigration, {
    # creating a new group using this button does not require an existing
    # name or group number... creating one from an existing project does
    addGroupFunctionMigration(NULL,NULL)
  })

  ###########################
  ###### TAB COMPLETED ######
  ###########################
  progressTracker$completed<<-'tab7'

  progressTracker$groupsChosen<<-TRUE

  stepNineFunction(input,output,session)
}
