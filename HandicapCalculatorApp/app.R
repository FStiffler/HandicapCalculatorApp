# load required packages
library(shiny)
library(tidyverse)

# load helper files
source("helperFiles/loadParameters.R")
source("helperFiles/helperFunctions.R")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Handicap Rechner"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "handicapIndex",
                         label = "Aktuelles Handicap",
                         value = 54,
                         min = 0,
                         max = 54,
                         step = 1),
            selectInput(inputId = "club",
                        label = "Golfklub",
                        choices = LIST_OF_CLUBS),
            selectInput(inputId = "tee",
                        label = "Abschlag",
                        choices = LIST_OF_TEES),
        ),

        # Show output
        mainPanel(
           tableOutput("courseHandicap")
        )
    )
)

# Define server logic
server <- function(input, output) {
  
  output$courseHandicap <- renderTable(
    
    exp = {
    
    # filter course data based on input values
    courseData <- COURSE_INFORMATION%>%
      filter(club==club&tee==input$tee)
    
    # calculate course handicap
    courseHandicap <- calculate_course_handicap(input$handicapIndex, courseData$courseRating, courseData$slopeRating, courseData$par)
    
    # create table
    res <- data.frame(values=c(as.character(input$handicapIndex),
                               as.character(courseData$courseRating),
                               as.character(courseData$slopeRating),
                               as.character(courseData$par),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
