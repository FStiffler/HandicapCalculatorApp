# load required packages
library(shiny)
library(tidyverse)
library(DT)

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
          DTOutput("courseInformationTable")
           
        )
    )
)

# Define server logic
server <- function(input, output) {
  
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
  newData <- reactive({
    
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
    courseInformation$data = newData()
    
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
    res <- data.frame(values=c(as.character(input$handicapIndex),
                               as.character(teeData()$courseRating),
                               as.character(teeData()$slopeRating),
                               as.character(teeData()$par),
                               as.character(courseHandicap())
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
  
}

# run the application 
shinyApp(ui = ui, server = server)
