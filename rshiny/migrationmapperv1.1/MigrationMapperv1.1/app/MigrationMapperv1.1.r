objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

dependencies<-c("shiny",'shinyBS',"leaflet","rgdal","rmapshaper","geojsonio",
"raster","dplyr","move","BBMM","snowfall","stringr","igraph","maptools","ggplot2","plotly",
"circular","devtools","scales","shinyjs","parallel")

# for(i in 1:length(dependencies)){
#   if(require(dependencies[i])){
#       print(paste0(dependencies[i]," is loaded correctly"))
#   } else {
#       print(paste0("trying to install ",dependencies[i]))
#       install.packages(dependencies[i])
#       if(require(dependencies[i])){
#           print(paste0(dependencies[i]," installed and loaded"))
#       } else {
#           stop(paste0("could not install ",dependencies[i]))
#       }
#   }
# }

for(i in 1:length(dependencies)){
  if(dependencies[i] %in% installed.packages()==FALSE){
    install.packages(dependencies[i])
    require(dependencies[i],character.only=TRUE)
  } else{
    require(dependencies[i],character.only=TRUE)
  }
}

options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
options(shiny.launch.browser = .rs.invokeShinyWindowViewer)

source("scripts/R_functions/creat.burst.R",local=TRUE)
source("scripts/R_functions/find.problem.pts.R",local=TRUE)
source("scripts/R_functions/mov.param.R",local=TRUE)
source("scripts/R_functions/mort.check.R",local=TRUE)

source("panelScripts/globals.R",local=TRUE)
source("panelScripts/configPage.R",local=TRUE)
source("panelScripts/tabOneFxns.R",local=TRUE)
source("panelScripts/tabTwoFxns.R",local=TRUE)
source("panelScripts/tabThreeFxns.R",local=TRUE)
source("panelScripts/tabFourFxns.R",local=TRUE)
source("panelScripts/tabFiveFxns.R",local=TRUE)
source("panelScripts/tabSixFxns.R",local=TRUE)
source("panelScripts/tabSevenFxns.R",local=TRUE)
source("panelScripts/tabEightFxns.R",local=TRUE)
source("panelScripts/tabNineFxns.R",local=TRUE)

ui <-
  fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode),
  navbarPage(
  theme = "sandstoneBootstrap.min.css",
  # "Migration Corridor Analysis Application",
  img(src='MigrationMapper_logo_whitebg.png', align = "right",height='60px',width='auto'),
  id="appTabPanels",
  ###------------
  ###------------
  ###TAB PANEL 1
  ###------------
  ###------------
  tabPanel(
    "1 - IMPORT DATA",
    value='panel1',
    # titlePanel("Collar Processing Application"),
    fluidRow(
      bsModal("welcomeModal", "", NULL, size = "large",
        HTML("
        <img src='wmi_logo.jpg' style='margin-left:5%; margin-right:10%; width:45%; height:auto;'>
        <img src='Gage_logo_black_wide.jpg' style='width:30%; height:auto;'>
        <img src='New_UWSignature_1_line_Brown_Web.png' style='margin-top:20px; margin-left:17%; width:66%; height:auto;'>
        <br>
        <br>
        <h1 style='text-align: center;'>Welcome to Migration Mapper - v1.0</h1>
        <p style='text-align: center;'>An application to facilitate analysis of animal movement and migration data</p>
        <p style='text-align: center;'>Please see the <a href='http://migrationinitiative.org/content/migration-mapper-user-guide'>User Guide</a> for more information</p>
        <br>
        "),
        actionButton("getStartedButton",align="center", style = "margin:0% 20% 0% 20%; width:60%; font-weight:bolder;", "Get Started!"),
        br(),
        br(),
        HTML("
          <img src='sponsors.jpg' style='margin-left:25%; width:50%; height:auto;'>
        "),
        br(),
        br(),
        p('© 2017 University of Wyoming. All Rights Reserved.'),

        # <img src='New_UWSignature_1_line_Brown_Web.png'  style='width:25%; height:auto;>
        # <img src='New_UWSignature_1_line_Brown_Web.png'  style='width:25%; height:auto;>
        # <br>
        # <img src='WAFWA-logo.png'  style='width:25%; height:auto;>

        # img(src='wmi_logo.jpg', align = "right",height='auto',width='60%'),
        # br(),
        # img(src='New_UWSignature_1_line_Brown_Web.png',style = "margin-top:18px;", align = "right",height='auto',width='60%'),
        # br(),
        # img(src='WAFWA-logo.png', align = "right",style = "margin-top:18px; margin-right:30%;",height='auto',width='30%'),
        # br(),
        # img(src='Gage_logo_black_wide.jpg',align = "right",style = "margin-top:18px;", align = "right",height='auto',width='40%'),
        br(),
        tags$style("#welcomeModal .modal-title{font-size:3.5rem;} #welcomeModal .modal-footer{ display:none}")
      ),
      bsModal("moreDataModal", "Merge data", NULL, size = "large",
        HTML("It appears you are merging data to an existing project or
        dataset. Be sure you are not importing duplicate individuals."),
        br(),
        br(),
        actionButton("noMergeData", "No - Cancel"),
        br(),
        br(),
        actionButton("yesMergeData","Yes - Import new data"),
        tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
      ),
      column(12,
        HTML(
          "You can upload one ESRI shapefile with many individuals, or multiple
          shapefiles each representing a single individual. If importing unique
          files for each individual, it is necessary that all files have identical
          columns, data formats and projections. If importing a merged file, it
          is necessary that the file includes a column delineating unique animal IDs.
          <br>
          <br>
          You also may way want to review the analysis parameters by clicking on the
          tab titled 'ANALYSIS PARAMETERS'. This tab gives you control over many of
          the variables that controls how analysis is run in the applicaiton.
          <br>
          <br>
          Using the button below, choose the directory containing
          your dataset(s). If you're uploading multiple files, they must
          all be in the same directory<br><br>
          "
        )
      ),
      column(4,
        strong('(1) Choose directory containing files to import by clicking the button below'),
        br(),
        actionButton("chooseDirButton", "Click to Choose Folder"),
        uiOutput("selectedDirectoryLabel"),
        uiOutput("fileUploadSelectorHolder"),
        uiOutput("fileUploadExecute"),
        strong('Uploaded File(s): Click to delete'),
        uiOutput("uploadedShapefile1"),
        uiOutput("uploadedShapefile2"),
        uiOutput("uploadedShapefile3"),
        uiOutput("uploadedShapefile4"),
        uiOutput("uploadedShapefile5"),
        uiOutput("uploadedShapefile6"),
        uiOutput("uploadedShapefile7"),
        uiOutput("uploadedShapefile8"),
        uiOutput("uploadedShapefile9"),
        uiOutput("uploadedShapefile10"),
        uiOutput("uploadedShapefile11"),
        uiOutput("uploadedShapefile12"),
        uiOutput("uploadedShapefile13"),
        uiOutput("uploadedShapefile14"),
        uiOutput("uploadedShapefile15"),
        uiOutput("uploadedShapefile16"),
        uiOutput("uploadedShapefile17"),
        uiOutput("uploadedShapefile18"),
        uiOutput("uploadedShapefile19"),
        uiOutput("uploadedShapefile20")
        ),
        column(4,
          uiOutput("workingDirectoryTitle"),
          br(),
          uiOutput("chooseWorkingDirButton"),
          uiOutput("selectedWorkingDirectoryLabel")
      ),
      column(4,
        uiOutput("uniqueIdTitle"),
        uiOutput("uniqueIdSelector"),
        uiOutput("uniqueIdSelectorGo"),
        uiOutput("selectedShapefileLabel")
        ),
      column(12,
        br(),
        br(),
        br(),
        br(),
        br(),
        p('If you have already started a project you can resume by clicking the
        button below and choosing your project folder. Note, that if you have
        shapefiles or other project files open in another program, you may
        experience errors. It is recomended that you close those files before
        resuming a project.'),
        actionButton("resumeProjectButton","Resume existing project"),
        br(),
        br(),
        HTML("<p>Please see the <a href='http://migrationinitiative.org/content/migration-mapper-user-guide'>User Guide</a> for more information</p>")
        )
      )
    ),
  ###------------
  ###------------
  ###TAB PANEL 2
  ###------------
  ###------------
  tabPanel("2 - DATE COLUMN(S)",
    value='panel2',
    fluidRow(
      column(4,
        p('Date/Time data comes in a variety of formats. We understand your
        date/time information could be stored in one column or spread out across
        several columns. To start, pick the column or columns from the list below
        that contain date/time information. Once you finish selecting the columns,
        click "DONE SELECTING DATE COLUMN(S)."
'),
        uiOutput("dateColumnSelector"),
        checkboxGroupInput(
          "dateColumnSelector",
          "",
          choiceNames=NULL,
          choiceValues=NULL,
          selected=NULL,
          inline=TRUE
        ),
        actionButton("doneChoosingDateColumnsButton", "DONE SELECTING DATE COLUMN(S).")
      ),
      column(8,align="right",
        tableOutput("dateConfigTable1")
      )
    )
  ),
  ###------------
  ###------------
  ###TAB PANEL 3
  ###------------
  ###------------
  tabPanel("3 - DATE CONFIGURATION",
  value='panel3',
  fluidRow(
    column(4,
      HTML('<p>The next step is select which date/time elements are contained
      in each column and in what order they appear. This is important so the App
      can interpret the date/time data correctly.
      <br><br>
      Shown here are menus that allow you to indicate which date/time elements
      are in each column you selected on the previous page. Using the dropdown
      menu chose the elements from each column in the exact order they appear.
      <br><br>
      Once you are ready to proceed click the "PROCESS DATES" button.
      <br><br>
      Note that when you click the process dates button, certain points and individuals
      may be removed from your dataset. For example, if an individual had unreasonable speeds
      or was categorized as mortalities, they could be removed from your dataset. It is recomended
      that you review the configuration tab before clicking the process dates button.
      </p>'
      ),
      uiOutput("timeProcessingResults")
    ),
    column(4,
      uiOutput("dateConfigUi1"),
      uiOutput("dateConfigUi2"),
      uiOutput("dateConfigUi3"),
      uiOutput("dateConfigUi4"),
      uiOutput("dateConfigUi5"),
      uiOutput("dateConfigUi6"),
      uiOutput("dateConfigUi7"),
      uiOutput("dateConfigUi8"),
      uiOutput("dateConfigUi9"),
      uiOutput("dateConfigUi10"),
      uiOutput("dateConfigUi11"),
      uiOutput("dateConfigUi12"),
      uiOutput("dateSeperatorSelector"),
      uiOutput("timeSeperatorSelector"),
      uiOutput("timezoneSelector"),
      uiOutput("timeFormatResults"),
      actionButton("processDatesButton", "PROCESS DATES")
    ),
    column(4,
      tableOutput("dateConfigTable")
    )
  )
  ),
  ##------------
  ###------------
  ###TAB PANEL 4
  ###------------
  ###------------
  tabPanel("4 - IMPORT RESULTS",
  value='panel4',
    fluidRow(
      column(12,
        p('Your data have been successfully imported. You can see the results
        of the import below. If any of these summaries seem off or unreasonable,
        you may want to check your dataset for errors, or change your import
        settings. After doing so, restart the application and try again.'),
        br(),
        br(),
        p('Your dataset was scanned for duplicate entries, mortalities,
        unreasonable speeds and problem points.'),
        br(),
        br(),
        p('Once you you are satisfied with the results, click tab
        "5-MIGRATION DATES" to continue.')
      ),
      column(12,
        uiOutput('totalIndividualsText'),
        uiOutput('totalIndividuals'),
        br(),
        br(),
        uiOutput('datesSummaryText'),
        uiOutput('datesSummary'),
        br(),
        br(),
        uiOutput('naDatesSummaryText'),
        uiOutput('naDatesSummary'),
        br(),
        br(),
        uiOutput('duplicatedPointsSummaryText'),
        uiOutput('duplicatedPointsSummary'),
        br(),
        br(),
        uiOutput('mortalitiesSummaryText'),
        uiOutput('mortalitiesSummary'),
        tableOutput("mortalitiesTable"),
        br(),
        br(),
        uiOutput('problemPointsSummaryText'),
        uiOutput('problemtPointsText'),
        br(),
        br(),
        uiOutput('speedPointsSummaryText'),
        uiOutput('speedPointsSummary'),
        tableOutput("speedPointsTable")
      )
    )
  ),
  ##------------
  ###------------
  ###TAB PANEL 5
  ###------------
  ###------------
  tabPanel("5 - MIGRATION DATES",
  value='panel5',
    fluidRow(
      column(12,
        p("It is now time to identify the dates of start and end of migration.
        Using the sliders, you will need to do this for both spring and fall
        migration, and for every individual in every year. If there is no migration
        present or the data look erroneous, click 'no spring or fall migration
        present.' After you have completed an animal/year, click
        'NEXT ANIMAL/YEAR' to move on. Once you've completed all animal/years
        in your dataset, click 'RUN BROWNIAN BRIDGE ANALYSIS' to go to the next step.")
      ),
      fluidRow(
        column(4, align="center", style = "margin-top:18px;",
          actionButton('backwardHandlerButton','Previous Animal/Year ←')
        ),
        column(4, align="center",
          selectInput(
            "currentIndividualSelector",
            "",
            c(1,2),
            selected = NULL,
            multiple = FALSE
          )
        ),
        column(4,align="center", style = "margin-top:18px;",
          actionButton('forwardHandlerButton','Next Animal/Year →')
        )
      ),
      column(12,
        br()
      ),
      column(6,
        # uiOutput("migrationPresentRadio")
        radioButtons("migrationPresentRadioSpring", "Does spring migration occur?",
               c("Yes, spring migration is present (you must select dates)" = TRUE,"No spring migration present, erroneous data, or missing data" = FALSE),
               selected=TRUE
        ),
        radioButtons("migrationPresentRadioFall", "Does fall migration occur?",
               c("Yes, fall migration is present (you must select dates)" = TRUE,"No fall migration present, erroneous data, or missing data" = FALSE),
               selected=TRUE
        )
      ),
      column(6, align="center",
        # uiOutput("dateSlider")
        sliderInput("dateSliderSpring",
          "Choose Spring Migration Start/End Dates:",
          min = as.Date("2017-01-01"), max = as.Date("2017-02-02"),
          value = c(as.Date("2017-01-01"),as.Date("2017-02-01")),
          width='90%'
        ),
        sliderInput("dateSliderFall",
          "Choose Fall Migration Start/End Dates:",
          min = as.Date("2017-01-01"), max = as.Date("2017-02-02"),
          value = c(as.Date("2017-01-01"),as.Date("2017-02-01")),
          width='90%'
        )
      ),
      column(12,
        br()
      ),
      column(6,
        leafletOutput("pointsMap")
        ),
      column(6,
        tabsetPanel(id="graphPanels",
          tabPanel("NSD Annual",value='graph1tab',
            plotOutput("plot1")
            ),
          tabPanel("NSD Spring",value='graph2tab',
            plotOutput("plot2")
            ),
          tabPanel("NSD Fall",value='graph3tab',
            plotOutput("plot3")
            )
        )
      )
    )
    ),
    ##------------
    ###------------
    ###TAB PANEL 6
    ###------------
    ###------------
    tabPanel("6 - BROWNIAN BRIDGE",
    value='panel6',
      fluidRow(
        bsModal("bbModal", "File delete warning", NULL, size = "large",
          HTML("You already have brownian bridge files in your project
          folder. If you proceed, files will be deleted and replaced
          with the results of this new run. Also, if you have already run
          final corridor merging, those files will be deleted."),
          br(),
          br(),
          actionButton("noRun", "No - Cancel"),
          br(),
          br(),
          actionButton("yesRun", "Yes - Delete and re-run BB"),
          tags$head(tags$style("#bbModal .modal-footer{ display:none}"))
        ),

        column(12,
          p('Your data have been successfully prepared for the Brownian Bridge
          analysis. Click the button below to start the Brownian Bridge process.
          This process can take a few minutes to several hours, depending on the sample size
          of your dataset and the size of your study area. Once analysis finishes
          you will see a map appear, plotting the results.'),
          br(),
          br(),
          p('If you have already run brownian bridge you may rerun the analysis
          by clicking the button below.')
        ),
        br(),
        column(12,
          actionButton('startBBButton','Start Brownian Bridge'),
          br(),
          br()
        ),
        column(12,
          # tags$style(type = "text/css", "#bbOutputMap {height: calc(100vh - 80px) !important;}"),
          tags$style(type = "text/css","#bbOutputMap .leaflet-control-layers-list{max-height:30vh !important; overflow:hidden !important; overflow-y:auto !important; max-width:400px !important; width:auto !important;}"),
          leafletOutput("bbOutputMap",height='75vh')
        ),
        br(),
        uiOutput("bbErrorsList")
      )
  ),
  ##------------
  ###------------
  ###TAB PANEL 7
  ###------------
  ###------------
  tabPanel("7 - ANIMAL GROUPING - MIGRATIONS",
  value='panel7',
    fluidRow(
      column(12,
        p('In preparation for calculating population-level migration corridors,
        you may need to specify that your dataset constitutes multiple populations.
        If so, add 2 or more group names, and then identify which group each
        individual is associated with using the dropdowns below. If your
        individuals are all from a single population and do not need to be
        organized into multiple groups, you can skip this step and proceed by
        clicking "9 - FINAL CORRIDORS."')
      ),
      br(),
      column(4, style = "overflow:scroll; overflow-y: scroll; max-height: 650px; padding-bottom:250px; margin-bottom:250px;",
        actionButton("addGroupButtonMigration", "+ Group"),
        uiOutput('animalGroupSectionMigration')
      ),
      column(8,
        tags$style(type = "text/css","#groupsMapMigration .leaflet-control-layers-list{max-height:30vh !important; overflow:hidden !important; overflow-y:auto !important; max-width:400px !important; width:auto !important;}"),
        leafletOutput("groupsMapMigration",height='75vh')
      )
    )
  ),
  ##------------
  ###------------
  ###TAB PANEL 8
  ###------------
  ###------------
  tabPanel("8 - ANIMAL GROUPING - WINTER",
  value='panel8',
    fluidRow(
      column(12,
        p('In preparation for calculating population-level migration corridors,
        you may need to specify that your dataset constitutes multiple populations.
        If so, add 2 or more group names, and then identify which group each
        individual is associated with using the dropdowns below. If your
        individuals are all from a single population and do not need to be
        organized into multiple groups, you can skip this step and proceed by
        clicking "9 - FINAL CORRIDORS."')
      ),
      br(),
      column(4, style = "overflow:scroll; overflow-y: scroll; max-height: 650px; padding-bottom:250px; margin-bottom:250px;",
        actionButton("addGroupButtonWinter", "+ Group"),
        uiOutput('animalGroupSectionWinter')
      ),
      column(8,
        tags$style(type = "text/css","#groupsMapWinter .leaflet-control-layers-list{max-height:30vh !important; overflow:hidden !important; overflow-y:auto !important; max-width:400px !important; width:auto !important;}"),
        leafletOutput("groupsMapWinter",height='75vh')
      )
    )
  ),
  ##------------
  ###------------
  ###TAB PANEL 9
  ###------------
  ###------------
  tabPanel("9 - FINAL CORRIDORS",
  value='panel9',
    fluidRow(
      column(12,
        bsModal("finalMergeModal", "File delete warning", NULL, size = "small",
          HTML("You already have final, merged corridors created in
          your project folder. If you proceed, these files will be
          deleted and replaced with the results of this merge"),
          br(),
          br(),
          actionButton("noMerge", "No - Cancel"),
          br(),
          br(),
          actionButton("yesMerge", "Yes - Delete and merge"),
          tags$head(tags$style("#finalMergeModal .modal-footer{ display:none}"))
        ),
        p('Brownian Bridge analysis has finished and groups have been selected.
        Click on "MERGE INDIVIDUALS" to proceed and individual corridors will
        be merged to create population level corridors.')
      ),
      br(),
      column(4,
        actionButton("mergeAnimalsButton", "Merge Animals")
      ),
      column(12,
        tags$style(type = "text/css","#finalMap .leaflet-control-layers-list{max-height:30vh !important; overflow:hidden !important; overflow-y:auto !important; max-width:400px !important; width:auto !important;}"),
        leafletOutput("finalMap",height='75vh')
      )
    )
  ),
  ##------------
  ###------------
  ###TAB PANEL CONFIGURATION
  ###------------
  ###------------
  tabPanel("ANALYSIS PARAMETERS",
  value='configPanel',
    fluidRow(
      column(12,
        p('This section has been created to allow to customize
        analysis parameters. Values are set to default,
        reasonable values and you can adjust as you see fit.'),
        h3('General Variables',style="background-color:black; color:white; padding:10px;")
      ),
      column(5,
        h4('Processing cores',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        uiOutput('cpusText'),
        selectInput(
          "cpuSelector",
          "",
          c(1,2),
          selected = NULL,
          multiple = FALSE
        ),
        br(),
        h4('Maximum Speed',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('You can change the maximum allowable speed below. Migration Mapper will
        remove GPS locations that are linked to unreasonable speeds (e.g.,
        because of collar malfunction or the collar being in the back of a truck).
        You can specify what that threshold is here. This has been set
        at a default value of 10.8 km/hr. Points are permanently removed from the project. After removing you would need to restart the project to bring them back.'),
        numericInput("maxSpeedSelector", "Max Speed (km/hr)", 10.8, step=0.1,)
      ),
      column(2,p()),
      column(5,
        h4('Mortality parameters',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('Migration Mapper will remove GPS locations that are linked to mortalities
        (i.e., the collar does not move a specified minimum distance over a specified
          amount of time). This is set to default at 50 meters and
        48 hours. You can adjust these values below if needed. Points are permanently removed from the project. After removing you would need to restart the project to bring them back.'),
        numericInput("mortDistance", "Minimum Distance (meters)", 50),
        numericInput("mortTime", "Time unit (hours)", 48),
        br(),
        h4('Displacement Type',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        radioButtons("nsdToPlotSelector",
           "Which type of displacement would you like to plot?",
            c("Net squared displacement" = TRUE,"Displacement" = FALSE),
            selected=TRUE)
      ),
      column(12,
        h3('Brownian Bridge Processing Variables',style="background-color:black; color:white; padding:10px;"),
        h4('Note that changing any of these variables after having run
        analysis will force a complete re-run of all individuals.')
      ),
      column(5,
        h4('Raster Resolution',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This is the cell size for which the Brownian Bridge Movement Analysis
        will use. The larger the value the faster the Brownian Bridge analysis runs.
        However, decreasing this number will provide a finer scale estimate of
        migration corridors and winter ranges.Resolution is set to a default value
        of 150 meters.'),
        numericInput("rasterResolutionSelector", "Cell Size (meters)", 150),
        br(),
        h4('Corridor Percentile',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This parameter represents the percentile for which a polygon is drawn around
        the utilization distribution calculated from the Brownian Bridge Movement
        analysis. The larger the number (i.e., closer to 1) the wider the corridors
        and winter ranges will be.'),
        numericInput("classifyPercentileSelector", "Classification Percentile", 0.99, step=0.00001, max=0.99999, min=0.95),
        br(),
        h4('Maximum fix interval (hours)?',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This is the maximum number of hours that can go between sequential GPS
        fixes before the Brownian Bridge Movement Analysis will not calculate a
        Brownian Bridge between them.'),
        numericInput("maxFixIntervalSelector", "Max fix interval (hours)", 8),
        br(),
        h4('Location Error',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This is the location error of your GPS collars. It is in meters.'),
        numericInput("locationErrorSelector", "Location error", 20, step=1, max=50, min=0),
        br()
      ),
      column(2,p()),
      column(5,
        h4('BB Margin',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This represents the number of GPS locations on each end of a given
        trajectory of GPS locations that are used as ‘burn-in’ for calculating the
        moving window of the Brownian motion variance in the Dynamic Brownian Bridge
        analysis. The larger the number, the more GPS locations at the edges of each
        movement track will be discarded in the Dynamic Brownian Bridge analysis.
        This parameter does not affect regular Brownian Bridge analysis.'),
        numericInput("bbMarginSelector", "dbb margin", 3, step=1, max=20, min=0),
        br(),
        h4('Window Size',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('This represents the width (in number of locations) of the moving window for
        calculating the Brownian motion variance in the Dynamic Brownian Bridge analysis.'),
        numericInput("windowSizeSelector", "Window size (DBB)", 31, step=2, max=31, min=3),
        h4('Brownian Bridge Type',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('You can choose between regular (Horne et al. 2007) or dynamic (Kranstauber et al. 2012) Brownian Bridge movement analyses.'),
        radioButtons("dbbRadioSelector",
          "Which type of Brownian Bridge would you like to run?",
          c("Dynamic Brownian Bridge" = TRUE,"Regular Brownian Bridge" = FALSE),
          selected=TRUE),
        br()
      ),
      column(12,
        h3('Winter Range Analysis',style="background-color:black; color:white; padding:10px;")
      ),
      column(5,
        h4('Calculate winter range?',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        p('Do you want Migration Mapper to also calculate population level winter ranges, or simply calculate migration corridors only?'),
        radioButtons("winterRangeYesNoRadio",
           "",
            c("No, don't calculate winter ranges" = FALSE,"Yes, calculate winter ranges" = TRUE),
            selected=FALSE),
        br(),
        h4('Winter Range Date Handling',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
        uiOutput("winterMinMaxText"),
        p('If you are calculating winter range, how
        would you like to account for individuals without a defined winter start
        and end date?'),
        radioButtons("winterStartEndRadio","",
          c(
            "Average start and end dates from other individuals" = 'mean',
            "Pick your own dates below" = 'chosen'
            ),selected='mean')
        ),
        column(2,p()),
        column(5,
          h4('Winter Range End Date (ignore year)',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
          dateInput("winterEndSelector",
            "Date:",
            value = "2017-02-29",
            format="mm-dd"
          ),
          h4('Winter Range End Date (ignore year)',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
          dateInput("winterStartSelector",
            "Date:",
            value = "2017-11-01",
            format="mm-dd"
          )
        )
      )
    )
  )
    )


server <- function(input, output, session) {
  globalInitFunctions(input, output, session)
  js$disableTab("panel2")
  js$disableTab("panel3")
  js$disableTab("panel4")
  js$disableTab("panel5")
  js$disableTab("panel6")
  js$disableTab("panel7")
  js$disableTab("panel8")
  js$disableTab("panel9")
  # js$disableTab("configPanel")
  stepOneFunction(input,output,session)
  configPageFunction(input,output,session)
  # stepSevenFunction(input,output,session)
  # stepEightFunction(input,output,session)
}

shinyApp(ui = ui, server = server)
