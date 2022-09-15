#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Necessary libraries

if (!require(install.load)){
  install.packages("install.load")
}
library(install.load)
install_load("shiny", "leaflet", "leafpop", "lattice", "DT", "tidyverse", "writexl")
print("Successfully loaded all packages!")

# Loading the dataset and ensure that the object will be a dataframe
dataset <- read_csv("Final_dataset_group_25.csv")
dataset <- data.frame(dataset)

# Create temporarily available folder
folder <- tempfile()
dir.create(folder)

# Define UI for application that shows a map, bar chart, the data table with all relevant columns as well as a download function
ui <- navbarPage(
         title = div(img(src="logo.png", height=35, width=35),
         "Defective registered diesel-engine vehicles"),
         tags$head(
           includeCSS("Additional_files/shiny.css")
         ),
                 
        # First clickable tap in the nav bar at the top of the application, which represents the map
        tabPanel("Map",
                 tags$head(
                   includeCSS("Additional_files/shiny.css")
                 ),
                 leafletOutput("map", width = "100%", height = "90vh"),
                 
                 # Movable window with a number and slider input for selecting the amount of radii and the size
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
        
        # Second clickable tap in the nav bar at the top of the application, which represents the bar chart
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
                   mainPanel(
                     plotOutput("bar_plot")
                   )
                 )
        ),
        
        # Third clickable tap in the nav bar at the top of the application, which represents the data table
        tabPanel("Table", 
                 div(class="allround_padding",
                     DTOutput("table")
                 ),
        ),
        
        # Fourth clickable tap in the nav bar at the top of the application, which represents the download option of the data table
        tabPanel("Download",
                 fluidPage(
                   selectInput("format",
                                "Select your download format",
                                choices = c(".csv", ".xlsx")
                   ),
                   downloadButton("downloadData", "Download data")
                 ),
        ),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Creation of vehicle data set (including defective & non-defective vehicles) sorted by "Gemeinden"
    vehicles <- reactive({
      dataset %>% 
        arrange(Gemeinden)
    })
    
    # Creation of only defective vehicle data set, which is also sorted by "Gemeinden"
    vehicles_defect <- reactive({
      dataset %>% 
        filter(Defect = TRUE) %>% 
        arrange(Gemeinden)
    })
    
    # Creating new data frame incorporating the amount of defective & non-defective vehicles for each radius
    plot_data <- reactive({
      
      # Define empty data frame with necessary columns
      df <- data.frame(Bucket = numeric(),
                       Defective = numeric(),
                       Non_Defective = numeric())
      
      # Loop through the amount of radii selected by the user
      for(x in 1:input$number){
        
        # Calculate the respective numbers
        defect_count <- vehicles() %>% 
          filter(Distance_KM <= x*input$radius & Defect == TRUE) %>% 
          count()
        
        non_defect_count <- vehicles() %>% 
          filter(Distance_KM <= x*input$radius & Defect == FALSE) %>% 
          count()
        
        # Add the data to the data frame
        df[nrow(df) + 1,] <- c(x*input$radius, defect_count$n, non_defect_count$n)
      }
      
      # Transform the data frame to accurately plot the data
      df <- df %>% gather(key="Defect_Y_N", value="Count", 2:3)
    })
    
    map_plot_data <- reactive({
      
      # Define empty data frame with necessary columns
      df <- data.frame(Gemeinden = character(),
                       Defect = logical(),
                       Total = numeric(),
                       Radius = numeric())
      
      # Loop through the amount of radii selected by the user
      for(x in 1:input$map_number){
        
        # Calculate the respective numbers
        community_data <- vehicles() %>% 
                            filter(Distance_KM <= x*input$map_radius) %>% 
                            group_by(Gemeinden,Defect) %>% 
                            summarise(Total = n()) %>% 
                            mutate(Radius = x*input$map_radius)
        
        # Add the data to the data frame
        df <- df %>% bind_rows(community_data)
      }
      df
    })
    
    map_data_vehicle <- reactive({
      
      # Define empty data frame with necessary columns
      df <- data.frame(ID_Fahrzeug = character(),
                       Gemeinden = character(),
                       Laengengrad = numeric(),
                       Breitengrad = numeric())
      
      # Loop through the amount of radii selected by the user
      for(x in 1:input$map_number){
        
        # Calculate the respective numbers
        round_data <- vehicles_defect() %>% 
                        filter(Distance_KM <= x*input$map_radius) %>% 
                        select("ID_Fahrzeug", "Gemeinden", "Laengengrad", "Breitengrad")
        
        # Add the data to the data frame
        df <- df %>% bind_rows(round_data)
      }
      df
    })
    
    create_map <- reactive({
      
      # Create a leaflet map with the created data frame in function "map_data_vehicle"
      the_map <- leaflet(data= map_data_vehicle()) %>% 
        addTiles() %>% 
        setView(lng = 13.3777, lat = 52.5162, zoom = 10) 
      
      #Load popup datasets
      registrations <- map_click_registrations()
      
      # Loop through the amount of radii selected by the user and add the concentric circle to the leaflet map object
      for(x in 1:input$map_number){
        the_map <- addCircles(map = the_map, data= the_map, lng = 13.3777, lat = 52.5162, radius = x*input$map_radius*1000)
      }
      the_map %>% 
        addCircleMarkers(
          layerId = ~Gemeinden, 
          group = x*input$map_radius,
          lng = ~Laengengrad, 
          lat = ~Breitengrad,
          radius = 6,
          color = "green",
          stroke = FALSE,
          fillOpacity = 0.5)
    })
    
    map_click_plot <- reactive({
      click <- input$map_marker_click
  
      if(is.null(click$id)){
        return()
      }else{
        filtered_chart <- map_plot_data() %>% 
                            filter(Gemeinden == click$id & Radius == click$group) %>% 
                            ggplot(aes(x=Defect, y=Total)) +
                              geom_col() +
                              geom_text(aes(label= Total), vjust = 1.5, colour = "white")
        
        return(filtered_chart)
      }
    })
    
    map_click_registrations <- reactive({
      click <- input$map_marker_click
      
      if(is.null(click$id)){
        return()
      }else{
        click_registrations <- map_plot_data() %>% 
                                filter(Gemeinden == click$id & Radius == click$group) %>% 
                                group_by(Gemeinden, Radius) %>% 
                                summarise(Total = sum(Total))
        
        return(click_registrations)
      }
    })
    
    # When map is clicked, show a popup with community info
    showPopup <- function(community, lat, lng) {
      registrations <- map_click_registrations()
      plot <- map_click_plot()
      svg(filename= paste(folder,"plot.svg", sep = "/"),
          width = 500 * 0.005, height = 300 * 0.005)
      print(plot)
      dev.off()
      
      content <- paste("<h3>Community:</h3>",
                       registrations$Gemeinden,
                       "<br>",
                       "<strong>Number of vehicles registered in this community:</strong>",
                       "<br>",
                       registrations$Total,
                       "<br>",
                       "<br>",
                       paste(readLines(paste(folder,"plot.svg",sep="/")), 
                       collapse = ""))
      
      leafletProxy("map") %>% addPopups(lng = lng, lat = lat, content, layerId = community)
    }
    
    observeEvent(input$map_marker_click, {
      leafletProxy("map") %>% clearPopups()
      click <- input$map_marker_click
  
      if(is.null(click$id)){
        return()
      }else{
        isolate({
          showPopup(click$id, click$lat, click$lng)
        })
      }
    })
    
    # Create the map output
    output$map <- renderLeaflet({  
      map <- create_map()
    })
    
    # Create the bar plot output
    output$bar_plot <- renderPlot({
        df <- plot_data()
      
        ggplot(df, aes(x=as.factor(Bucket), y=Count, fill=Defect_Y_N))+
          geom_bar(stat = "identity", position = position_dodge()) + 
          geom_text(aes(label=Count), vjust=1.6, color="white",
                    position = position_dodge(0.9), size=4) +
          ggtitle("Affected/non-affected vehicles per radius") +
          ylab("Number of diesel-engined vehicles") +
          xlab("Radius in km") +
          scale_fill_discrete(name="State", labels = c("affected", "uneffected")) +
          theme(
            plot.title = element_text(size = 16, face = "bold.italic", hjust = 0.5),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12)
          )
    })
    
    # Create the download output
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), input$format, sep="")
      },
      
      # Check which data format needs to be written
      content = function(file) {
        if(input$format == ".csv"){
          write_csv(vehicles(), file)  
        }else if (input$format == ".xlsx"){
          write_xlsx(vehicles(), file)
        }
      }
    )
    
    # Create the data table output
    output$table <- renderDT({
      vehicles()
    })
}
 

# Run the application 
shinyApp(ui = ui, server = server)
