#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(writexl)

dataset <- read_csv("Final_dataset_group_25.csv")
dataset <- data.frame(dataset)

# Define UI for application that draws a histogram
ui <- navbarPage("Defective registered diesel-engine vehicles",
        tabPanel("Map",
                 tags$head(
                   includeCSS("shiny.css")
                 ),
                 leafletOutput("map", width = "100%", height = "90vh"),
                 absolutePanel(id = "inputs", style="z-index:500;", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                               top = 90, left = "auto", right = 30, bottom = "auto", width = 350, height = "auto",
                               
                               div(class="panel_padding",
                                 h3("Overview defective vehicles"),
                                 
                                 numericInput("number",
                                              "Number of radii:",
                                              value = 2,
                                              min = 1,
                                              max = 50
                                 ),
                                 sliderInput("radius",
                                             "Size of radius:",
                                             value = 10,
                                             min = 1,
                                             max = 50
                                 ),
                               ),
                 ),
                               
        ),
        tabPanel("Bar Chart",
                 # Sidebar with a number and slider input for selecting the amount of radii and the size
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("number",
                                  "Number of radii:",
                                  value = 2,
                                  min = 1,
                                  max = 50
                     ),
                     sliderInput("radius",
                                 "Size of radius:",
                                 value = 10,
                                 min = 1,
                                 max = 50
                     ),
                   ),
                   
                   # Display a map, bar chart, download option and data table in different tabs
                   mainPanel(
                     plotOutput("bar_plot"),
                   )
                 )
        ),
        tabPanel("Download",
                 fluidPage(
                   selectInput("format",
                                "Select your download format",
                                choices = c(".csv", ".xlsx")
                   ),
                   downloadButton("downloadData", "Download data")
                 ),
        ),
        tabPanel("Table", 
                 div(class="allround_padding",
                     DTOutput("table")
                ),
        ),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    vehicles <- reactive({
      dataset %>% 
        arrange(Gemeinden)
    })

    output$map <- renderLeaflet({
        leaflet(data= vehicles()) %>% 
          addTiles() %>% 
          setView(lng = 13.3777, lat = 52.5162, zoom = 10) %>% 
          addCircleMarkers(lng = ~ Laengengrad, lat = ~ Breitengrad)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), input$format, sep="")
      },
      content = function(file) {
        if(input$format == ".csv"){
          write_csv(vehicles(), file)  
        }else if (input$format == ".xlsx"){
          write_xlsx(vehicles(), file)
        }
      }
    )
    
    output$table <- renderDT({
      vehicles()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
