# load required packages
library(shiny)
library(tidyverse)
library(DT)
library(rhandsontable)

# load helper files
source("helperFiles/loadParameters.R")
source("helperFiles/helperFunctions.R")

# define UI ----
ui <- fluidPage(
  
    # include custom style sheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css")
    ),
    
    # create page with navbar
    navbarPage("Handicap Rechner",
               
               # create first tab for calculation of score differential ----
               tabPanel("Runden Rechner - Score Differential",
                        
                        # first row for input parameters 
                        fluidRow(
                          
                          # first column element 
                          column(1,
                                 
                                 # numeric input for handicap index
                                 numericInput(inputId = "handicapIndex",
                                              label = "Handicap",
                                              value = 54,
                                              min = 0,
                                              max = 54,
                                              step = 1)
                          ),
                          
                          # second column element 
                          column(2,
                                 
                                 # single choice for golf club selection
                                 selectInput(inputId = "club",
                                             label = "Golfklub",
                                             choices = LIST_OF_CLUBS)
                          ),
                          
                          # third column element 
                          column(2,
                                 
                                 
                                 # single choice for tee selection
                                 selectInput(inputId = "tee",
                                             label = "Abschlag",
                                             choices = LIST_OF_TEES)
                          )
                        ),
                        
                        # second row for course information 
                        fluidRow(
                                 
                                 # output table with general tee information depending on inputs
                                 h4(textOutput("generalTeeInformationTitle"))
                                 
                        ),
                        
                        # third row for course information 
                        fluidRow(
                                 
                                 tableOutput("generalTeeInformationTable")
                        ),
                        
                        # fourth row for course information 
                        fluidRow(
                          
                          # first column element 
                          column(6, 
                                 
                                 # output table with general information about the selected course
                                 DTOutput("courseInformationTable")
                          ),
                          
                          # second column element 
                          column(6,
                                 
                                 # first fluid row inside column
                                 fluidRow(
                                   
                                   # output text with summary stats
                                   htmlOutput("summaryStats"),
                                   
                                 ),
                                 
                                 # second fluid row inside column
                                 fluidRow(
                                   
                                   # output text with score differential
                                   htmlOutput("scoreDifferential")
                                   
                                   )
                                 )
                          )
                        ),
               
               # create second tab for calculation of handicap ----
               tabPanel("Handicap Rechner",
                        
                        # first row for input parameters
                        fluidRow(
                                 
                                 # select player
                                 selectInput(inputId="player",
                                             label="Spieler wählen",
                                             choices=LIST_OF_PLAYERS)
                                 
                                 ),
                        
                        # third row for output table
                        fluidRow(
                          
                          # show handicap results of selected player
                          h4("Stammblatt"),
                          
                          # show handicap results of selected player
                          rHandsontableOutput("handicapResults")
                          
                          )
                        ),
               
               # create a new area with app management options ----
               navbarMenu("Einstellungen",
                          
                          # create new area with player management options ----
                          tabPanel("Spieler Management",
                                   
                                   # second row for input parameters
                                   fluidRow(
                                     
                                     # first subrow
                                     fluidRow(
                                       
                                       # first column
                                       column(2, 
                                              
                                              # titel
                                              h4("Spieler Hinzufügen:"),
                                              
                                              # text input for name
                                              textInput("newPlayerName", "Name des Spielers")
                                              
                                       ),
                                       
                                       # second column
                                       column(2, 
                                              
                                              actionButton(inputId="addPlayer",
                                                           label="Spieler Hinzufügen"
                                              )
                                              
                                       )
                                     )
                                   ),
                                   
                                   # third row for input parameters
                                   fluidRow(
                                     
                                     # first subrow
                                     fluidRow(
                                       
                                       # first column
                                       column(2, 
                                              
                                              # titel
                                              h4("Spieler Löschen:"),
                                              
                                              # select input for player to be deleted
                                              selectInput(inputId="playerToDelete",
                                                          label="Name des Spielers",
                                                          choices=c("",LIST_OF_PLAYERS))
                                              
                                       ),
                                       
                                       # second column
                                       column(2, 
                                              
                                              actionButton(inputId="deletePlayer",
                                                           label="Spieler Löschen")
                                              )
                                       )
                                     )
                                   )
                          )
               )
    )


# define server logic ----
server <- function(input, output) {
  
  # server logic for score differential calculation ----
  
  ## reactive variables
  
  # reactive table with current tee information which immediately updates when inputs change
  teeData <- reactive({
    
    # filter tee information based on inputs
    TEE_INFORMATION%>%
      filter(club==input$club&tee==input$tee)
    
    })
  
  # reactive variable to calculate course handicap which immediately updates when inputs change
  courseHandicap <- reactive({
    
    # calculate course handicap
    calculate_course_handicap(input$handicapIndex, teeData()$courseRating, teeData()$slopeRating, teeData()$par)
    
  })
  
  
  # initialize empty reactive value as placeholder for course information to be updated when user interacts with UI
  courseInformation <- reactiveValues(data=NULL)
  
  # reactive table with course information which is updated whenever user updates input variables
  newCourseData <- reactive({
    
    # filter course data based on input values
    newCourseData <- COURSE_INFORMATION%>%
      filter(club==club&tee==input$tee)
    
    # calculate additional strokes per hole based on course handicap and add them to table
    newCourseData<-calculate_additional_strokes(newCourseData, courseHandicap())
    
    # calculate additional fields
    newCourseData<-newCourseData%>%
      mutate(nettoPar = par+additionalStrokes)%>%
      mutate(strokes = nettoPar)%>%
      mutate(overPar = strokes-par,
             overNettoPar = strokes-nettoPar)%>%
      mutate(stablefordPoints = map_dbl(overNettoPar, calculate_stableford_points))%>%
      select(-club, -tee)
    
    # print value
    newCourseData
    
  })
  
  # as soon as any changes in the input parameters happen, assign the updated table as reactive value
  observe({
    
    # update course information data
    courseInformation$data = newCourseData()
    
  })
  
  # reactive summary stats table
  summaryStats<-reactive({
    
    courseInformation$data%>%
      summarise(
        strokesTotal = sum(strokes),
        overParTotal = sum(overPar),
        overNettoParTotal = sum(overNettoPar),
        stablefordPointsTotal = sum(stablefordPoints)
      )
    
  })
  
  
  ## output elements
  
  # create title with selected club and tee as information
  output$generalTeeInformationTitle <- renderText(
    
    expr = {paste0(input$club,", Abschlag ",input$tee)}
    
  )
  
  
  # create output table with general information about selected tee
  output$generalTeeInformationTable <- renderTable(
    
    # code to create table
    exp = {
    
    # create table
    teeData()%>%
        select(par, courseRating, slopeRating)%>%
        mutate_if(is.double, as.character)%>%
        rename(PAR=par, "Course Rating"=courseRating, "Slope Rating"=slopeRating)%>%
        add_column(Spielvorgabe=as.character(courseHandicap()))
    
    }
    )
  
  # create output table with course information per hole
  output$courseInformationTable <- renderDT(
      
      # print final table
      datatable(courseInformation$data%>%
                  rename("Loch"=hole, "Hcp."=hcp, "PAR"=par, "Vorgabe"=additionalStrokes, "Netto-PAR"=nettoPar, "Schläge"=strokes, "Über PAR"=overPar,
                                                "Über Netto-PAR"=overNettoPar, "Stableford-Punkte Netto"=stablefordPoints),
                rownames = FALSE,
                filter = "none", # no filter options
                selection = "none", # no selection of rows
                options = list(pageLength = 18, # show all entries
                               dom="t" # show only table (no search field, no page scroll, etc.)
                               ),
                editable = TRUE
                )
  )
  
  # Recalculate table values, when the number of strokes is changed 
  observeEvent(input$courseInformationTable_cell_edit, {
    
    # when cell is edited, create 
    info <- input$courseInformationTable_cell_edit
    str(info)
    i = info$row
    print(i)
    j = info$col
    print(j)
    v = info$value
    print(v)
    
    # describe what happens in case of event
    courseInformation$data[i, j+1]<<-v # overwrite value in table with user input
    courseInformation$data<-courseInformation$data%>% # recalculate values
                              mutate(overPar = strokes-par,
                                     overNettoPar = strokes-nettoPar)%>%
                              mutate(stablefordPoints = map_dbl(overNettoPar, calculate_stableford_points))
    
  })
  
  # create summary Stats
  output$summaryStats<-renderUI({
    
    HTML(paste("
               <div>
                <div class='statName'>Anzahl Schläge</div>
                <div class='statType1'>",summaryStats()$strokesTotal,"</div>
               </div>
               <div>
                <div class='statName'>Über PAR</div>
                <div class='statType1'>",summaryStats()$overParTotal,"</div>
               </div>
               <div>
                <div class='statName'>Über Netto-PAR</div>
                <div class='statType1'>",summaryStats()$overNettoParTotal,"</div>
              </div>
              <div>
                <div class='statName'>Stabelford Punkte</div>
                <div class='statType1'>",summaryStats()$stablefordPointsTotal,"</div>
              </div>"
               ))
    
    
  })
  
  # calculate score differential
  output$scoreDifferential<-renderUI({
    
    # load relevant variables 
    par<-teeData()$par
    courseRating<-teeData()$courseRating
    slopeRating<-teeData()$slopeRating
    courseHandicap<-courseHandicap()
    stablefordPoints<-summaryStats()$stablefordPointsTotal
    
    scoreDifferential<-round((par+courseHandicap-(stablefordPoints-36)-courseRating)/slopeRating*113,1)
    
    HTML(paste("
              <div>
                <div class='statName'>Score Differential</div>
                <div class='statType2'>",scoreDifferential,"</div>
               </div>
               "))
    
  })
  
  
  # server logic for handicap calculation ----
  
  # create reactive expression to filter handicap results
  handicapResults <- reactive({
    HANDICAP_RESULTS%>%
      filter(player==input$player)
  })
  
  # show handicap results
  output$handicapResults <- renderRHandsontable({
    
    # show handicap results
    rhandsontable(handicapResults()%>%
                    select(-player)%>%
                    rename(Ort = club, Abschlag = tee, Datum = date, "Score Differential" = scoreDifferential),
                  stretchH = "all")
    
  })
  
  # server logic for player management ----
  
  # player list as reactive value
  listOfPlayers<-reactiveVal(LIST_OF_PLAYERS)
  
  # add player to list
  observeEvent(input$addPlayer, {
    
    # create new list of players
    newList <- c(listOfPlayers(), input$newPlayerName)
    
    # make it the new list
    listOfPlayers(newList)
    
    # update select inputs
    updateSelectInput(inputId="player", choices = listOfPlayers())
    updateSelectInput(inputId="playerToDelete", choices = c("",listOfPlayers()))
    
    # remove textinput from input field
    updateTextInput(inputId = "newPlayerName", value = "")
    
    # save new player in players list
    write_csv(data.frame(player=listOfPlayers()), "data/listOfPlayers.csv")
    
    # show notification that player was successfully added
    showNotification("Neuer Spieler erfolgreich hinzugefügt", closeButton = TRUE)
    
  })
  
  # delete player from list
  observeEvent(input$deletePlayer, {
    
    # create new list of players
    newList <- listOfPlayers()[listOfPlayers() != input$playerToDelete]
    
    # make it the new list
    listOfPlayers(newList)
    
    # update select inputs
    updateSelectInput(inputId="player", choices = listOfPlayers())
    updateSelectInput(inputId="playerToDelete", choices = c("",listOfPlayers()))
    
    # save new player in players list
    write_csv(data.frame(player=listOfPlayers()), "data/listOfPlayers.csv")
    
    # show notification that player was successfully deleted
    showNotification("Spieler wurder erfolgreich gelöscht", closeButton = TRUE)
    
  })
  
  
}

# run the application ----
shinyApp(ui = ui, server = server)
