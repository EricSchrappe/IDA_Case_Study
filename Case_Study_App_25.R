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
                                 
                                 numericInput("map_number",
                                              "Number of radii:",
                                              value = 2,
                                              min = 1,
                                              max = 50
                                 ),
                                 sliderInput("map_radius",
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
                                  "Number of radi:",
                                  value = 4,
                                  min = 1,
                                  max = 50,
                                  step = 1
                     ),
                     sliderInput("radius",
                                 "Size of radius:",
                                 value = 25,
                                 min = 1,
                                 max = 50
                     ),
                   ),
                   
                   # Display a map, bar chart, download option and data table in different tabs
                   mainPanel(
                     plotOutput("bar_plot")
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
    
    vehicles_defect <- reactive({
      dataset %>% 
        filter(Defect = TRUE) %>% 
        arrange(Gemeinden)
    })
    
    plot_data <- reactive({
      df <- data.frame(Bucket = numeric(),
                       Defective = numeric(),
                       Non_Defective = numeric())
      
      for(x in 1:input$number){
        
        defect_count <- vehicles() %>% 
          filter(Distance_KM <= x*input$radius & Defect == TRUE) %>% 
          count()
        
        non_defect_count <- vehicles() %>% 
          filter(Distance_KM <= x*input$radius & Defect == FALSE) %>% 
          count()
        
        df[nrow(df) + 1,] <- c(x*input$radius, defect_count$n, non_defect_count$n)
      }
      
      df <- df %>% gather(key="Defect_Y_N", value="Count", 2:3)
    })
    
    map_data_vehicle <- reactive({
      df <- data.frame(ID_Fahrzeug = character(),
                       Gemeinden = character(),
                       Laengengrad = numeric(),
                       Breitengrad = numeric())
      
      for(x in 1:input$map_number){
        round_data <- vehicles_defect() %>% 
                        filter(Distance_KM <= x*input$map_radius) %>% 
                        select("ID_Fahrzeug", "Gemeinden", "Laengengrad", "Breitengrad")
        
        df <- df %>% bind_rows(round_data)
      }
      df
    })
    
    create_map <- reactive({
      the_map <- leaflet(data= map_data_vehicle()) %>% 
        addTiles() %>% 
        setView(lng = 13.3777, lat = 52.5162, zoom = 10) %>% 
        addCircleMarkers(
          lng = ~Laengengrad, 
          lat = ~Breitengrad,
          radius = 6,
          color = "green",
          stroke = FALSE,
          fillOpacity = 0.5)
      
      for(x in 1:input$map_number){
        the_map <- addCircles(map = the_map, data= the_map, lng = 13.3777, lat = 52.5162, radius = x*input$map_radius*1000)
      }
      the_map
    })
    

    output$map <- renderLeaflet({
        map <- create_map()
    })
    
    
    output$bar_plot <- renderPlot({
        df <- plot_data()
      
        ggplot(df, aes(x=as.factor(Bucket), y=Count, fill=Defect_Y_N))+
          geom_bar(stat = "identity", position = "dodge")
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
