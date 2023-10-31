# load required packages
library(shiny)
library(tidyverse)

# load helper files
source("helperFiles/loadParameters.R")
source("helperFiles/helperFunctions.R")

# define UI 
ui <- fluidPage(

    # application title
    titlePanel("Handicap Rechner"),

    # sidebar
    sidebarLayout(
      
        # input panel
        sidebarPanel(
            
            # numeric input for handicap index
            numericInput(inputId = "handicapIndex",
                         label = "Aktuelles Handicap",
                         value = 54,
                         min = 0,
                         max = 54,
                         step = 1),
            
            # single choice for golf club selection
            selectInput(inputId = "club",
                        label = "Golfklub",
                        choices = LIST_OF_CLUBS),
            
            # single choice for tee selection
            selectInput(inputId = "tee",
                        label = "Abschlag",
                        choices = LIST_OF_TEES),
        ),

        # output panel
        mainPanel(
          
          # output table with general tee information depending on inputs
          h4(textOutput("generalTeeInformationTitle")),
          tableOutput("generalTeeInformationTable"),
          
          # output table with general information about the selected course
          tableOutput("courseInformationTable")
           
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  
  # create title with selected club and tee as information
  output$generalTeeInformationTitle <- renderText(
    
    expr = {paste0(input$club,", Abschlag ",input$tee)}
    
  )
  
  
  # create output table with general information about selected tee
  output$generalTeeInformationTable <- renderTable(
    
    # code to create table
    exp = {
    
    # filter tee data based on input values
    teeData <- TEE_INFORMATION%>%
      filter(club==club&tee==input$tee)
    
    # calculate course handicap
    courseHandicap <- calculate_course_handicap(input$handicapIndex, teeData$courseRating, teeData$slopeRating, teeData$par)
    
    # create table
    res <- data.frame(values=c(as.character(input$handicapIndex),
                               as.character(teeData$courseRating),
                               as.character(teeData$slopeRating),
                               as.character(teeData$par),
                               as.character(courseHandicap)
                               )
                      )
    rowNames<-c("Aktuelles Handicap", "Course Rating", "Slope Rating", "PAR", "Spielvorgabe")
    row.names(res)<-rowNames
    
    # print final table
    res
    
    },
    
    # adjust other table parameters
    rownames = TRUE,
    colnames = FALSE,
    striped = TRUE
    )
  
  # create output table with course information per hole
  output$courseInformationTable <- renderTable(
    
    # code to create table
    exp = {
      
      # this table requires the courseHandicap
      req(courseHandicap)
      
      # filter course data based on input values
      courseData <- COURSE_INFORMATION%>%
        filter(club==club&tee==input$tee)
      
      # calculate additional strokes per hole based on course handicap
      courseData<-calculate_additional_strokes(courseData, courseHandicap)
      
      # table cosmetics
      courseData<-courseData%>%
        select(-club, -tee)%>%
        rename("Loch"=hole, "Hcp."=hcp, "PAR"=par, "Vorgabe"=additionalStrokes)
      
      # print final table
      courseData
      
    },
    
    # adjust other table parameters
    rownames = FALSE,
    colnames = TRUE,
    striped = TRUE,
    digits = 0
  )
  
}

# run the application 
shinyApp(ui = ui, server = server)
