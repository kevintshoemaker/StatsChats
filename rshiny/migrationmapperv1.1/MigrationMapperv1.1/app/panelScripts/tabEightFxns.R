stepEightFunction<-function(input,output,session){

  if('groupNamesMembersWinter' %in% names(progressTracker)){
    groupNamesMembersWinter<<-progressTracker$groupNamesMembersWinter
  }


  # CHOOSING GROUPSm

  if(stepEightHappened){
    return()
  }

  stepEightHappened<<-TRUE

  existingPolygonsMappedTab8<<-FALSE

  groupsObjectWinter<<-c()

  # migTimeTable<-read.csv(file.choose())
  groupNumber<-0

  # groupColors<-c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
  groupColors<-c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

  updateSelectMenus<-function(){
    if(length(polygonsHolderWinter)==0){
      return()
    }
    for(i in 1:length(polygonsHolderWinter)){
      thisIndividual<-names(polygonsHolderWinter[i])
      whichSelected<-input[[paste0(thisIndividual,'groupWinter')]]
      if(is.null(whichSelected)){
        return()
      }
      # if this one is already selected we might have to change the selection
      # as the name is updated
      if(nchar(whichSelected)>0){
        # this is the position in the groups object from the names/members
        thisGroupNumber<-as.numeric(groupNamesMembersWinter[[thisIndividual]][1])
        # and the name value for resetting the select
        whichSelected<-groupsObjectWinter[thisGroupNumber]
      }
      updateSelectInput(session=session, paste0(thisIndividual,'groupWinter'),
        choices = groupsObjectWinter,
        selected = whichSelected
      )
    }

  }

  addGroupFunctionWinter<-function(groupName,groupNumber){
    if(is.null(groupName) & is.null(groupNumber)){
      # if there are already groups
      if(length(groupsObjectWinter)>0){
        # make sure the last one has a name
        if(groupsObjectWinter[length(groupsObjectWinter)]==""){
          modalMessager('','Make sure to choose a name for the group you created')
          return()
        }
      }
      thisGroupNumber<-length(groupsObjectWinter)+1
      tempInputValue<-''
      groupsObjectWinter[thisGroupNumber]<<-""
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
    #   selector = paste0("groupLabelWinter",thisGroupNumber)
    # )

    removeUI(selector = paste0(".shiny-input-container:has(>#",paste0("groupLabelWinter",thisGroupNumber),")"))
    # removeUI(selector = paste0('.shiny-input-container:has(#%s)',paste0("groupLabelWinter",thisGroupNumber)))

    insertUI(
      selector = "#addGroupButtonWinter",
      where = "afterEnd",
      ui = textInput(
        inputId=paste0("groupLabelWinter",thisGroupNumber),
        label=paste("Choose Name for Group",thisGroupNumber),
        value=tempInputValue
        )
    )

    # this observer watches changes to the group names
    # if there is a change then all the select menu values are updated
    observeEvent(input[[paste0("groupLabelWinter",thisGroupNumber)]], {
    # observe({
      # tempGroupName<-thisInputValue()
      tempGroupName<-input[[paste0("groupLabelWinter",thisGroupNumber)]]

      if(!is.null(tempGroupName)){
        if(nchar(tempGroupName)>0){
          groupsObjectWinter[thisGroupNumber]<<-c(thisGroupNumber=tempGroupName)
          progressTracker$groupsObjectWinter<<-groupsObjectWinter
          updateSelectMenus()
        }
      }
    },ignoreInit=TRUE)
  }

  addExistingGroupsWinter<<-function(){
    if('groupsObjectWinter' %in% names(progressTracker)){
      groupsObjectWinter<<-progressTracker$groupsObjectWinter
      for(i in 1:length(groupsObjectWinter)){
        addGroupFunctionWinter(groupsObjectWinter[i],i)
      }
      progressTracker$completed<<-'tab8'
    }
  }
  addExistingGroupsWinter()


  #attach observers to select menus
  selectObserver<-function(thisIndividual){

    print(thisIndividual)

    observeEvent(input[[paste0(thisIndividual,'groupWinter')]], {
      # currently selected group
      selectedGroup<-input[[paste0(thisIndividual,'groupWinter')]]
      if(selectedGroup==""){
        if(thisIndividual %in% names(groupNamesMembersWinter)){
          groupNamesMembersWinter[[thisIndividual]]<<-NULL
          progressTracker$groupNamesMembersWinter<<-groupNamesMembersWinter
          saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
          mapIndividualsGroupPageWinter()
        }
      }
      # if(selectedGroup==""){
      #   groupNamesMembersWinter[[thisIndividual]]<<-c(99,'noGroup')
      # }
      # which position in group names is this group?
      groupPosition<-match(selectedGroup,groupsObjectWinter)
      if(!is.na(groupPosition)){

        # when resuming an existing project, when all select menus
        if(thisIndividual %in% names(groupNamesMembersWinter)){
          if(groupNamesMembersWinter[[thisIndividual]]==c(groupPosition,selectedGroup)){
            return()
          }
        }

        groupNamesMembersWinter[[thisIndividual]]<<-c(groupPosition,selectedGroup)
        progressTracker$groupNamesMembersWinter<<-groupNamesMembersWinter
        saveRDS(progressTracker, file=paste0(workingFilesDirectory,"\\", 'progressFile.rds'))
        mapIndividualsGroupPageWinter()
      }
    },ignoreInit=TRUE)
  }


  # for each of the individuals, create a select menu
  createSelectMenusWinter<<-function(){
    print('create select menus winter')
    if(length(polygonsHolderWinter)==0){
      print('add polygons winter')
      return()
    }
    for(i in 1:length(polygonsHolderWinter)){
      print('creating menu')
      print(i)
      thisIndividual<-names(polygonsHolderWinter[i])
      selectedValue<-NULL
      # check if this individual has already been assigned a group
      if(thisIndividual %in% names(progressTracker$groupNamesMembersWinter)){
        selectedValue<-progressTracker$groupNamesMembersWinter[[thisIndividual]][2]
      }
      selectValues<-c('')
      if(length(groupsObjectWinter)>0){
        selectValues<-c('',groupsObjectWinter)
      }


      removeUI(
        selector = sprintf('.shiny-input-container:has(#%s)',paste0(thisIndividual,'groupWinter'))
      )

      insertUI(
        selector = "#animalGroupSectionWinter",
        where = "afterEnd",
        ui = selectInput(
              paste0(thisIndividual,'groupWinter'),
              thisIndividual,
              selectValues,
              selected = selectedValue,
              multiple = FALSE
          )
      )
      selectObserver(thisIndividual)
    }
    tab8MenusCreated<<-TRUE

  }
  if(!tab8MenusCreated){
    createSelectMenusWinter()
  }






  ## to do
  ## add rows below map for each animal
  ## each time a group is added, add checkboxes or multiselect for individual

  output$groupsMapWinter <- renderLeaflet({
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
  observeEvent(input$groupsMapWinter_zoom,{
      if(!mapRendered){
        mapRendered<<-TRUE
        mapIndividualsGroupPageWinter()
        leafletProxy("groupsMapWinter") %>%
          fitBounds(minLonWin,minLatWin,maxLonWin,maxLatWin)
      }
  })

  mapIndividualsGroupPageWinter<<-function(){
    if(length(polygonsHolderWinter)==0){
      return()
    }
    leafletProxy("groupsMapWinter")%>%
      clearShapes()

    for(i in 1:length(polygonsHolderWinter)){
      tempPoly<-polygonsHolderWinter[[i]]
      tempPolyName<-names(polygonsHolderWinter[i])

      thisFillColor<-'#232928'
      if(tempPolyName %in% names(groupNamesMembersWinter)){
        thisFillColor<-groupNamesMembersWinter[[tempPolyName]][1]
        thisFillColor<-groupColors[as.numeric(thisFillColor)]
      }
      leafletProxy("groupsMapWinter") %>%
        addPolygons(data=tempPoly,
          fillColor=thisFillColor,
          fillOpacity=0.65,
          color='#232928',
          weight=1.5,
          popup=tempPolyName,
          group=tempPolyName
          )
    }
    leafletProxy("groupsMapWinter") %>%
      addLayersControl(
        overlayGroups = names(polygonsHolderWinter),
        baseGroups = c("NatGeo Base","Satellite","OSM (default)"),
        options = layersControlOptions(collapsed = TRUE))
  }

  observeEvent(input$addGroupButtonWinter, {
    # creating a new group using this button does not require an existing
    # name or group number... creating one from an existing project does
    addGroupFunctionWinter(NULL,NULL)
  })

  ###########################
  ###### TAB COMPLETED ######
  ###########################
  progressTracker$completed<<-'tab7'

  progressTracker$groupsChosen<<-TRUE

  stepNineFunction(input,output,session)
}
