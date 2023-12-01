library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Read the data (replace with your actual data file)
data <- read.csv("F1 2010 - JB McLaren Telemetry, Australia - Sheet1.csv")

# Define UI
# Define UI
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "IITKMS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lap_Analysis", tabName = "Lap_Analysis", icon = icon("car")),
      menuItem("Acceleration Graphs", tabName = "AccGraph", icon = icon("car")),
      menuItem("Live Analysis", tabName = "LiveAnalysis", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Lap_Analysis",
        h2("Choose Lap"),
        box(
          selectInput("dropdown2", label = "Choose Sessions", choices = unique(data$Lap), multiple = TRUE, selected = c(1, 2))
        ),
        box(
          title = "Average Speed",
          status = "info",
          solidHeader = TRUE,
          width = 4,
          height = 150,
          textOutput("avgSpeedInfo")
        ),
        box(
          plotOutput("mapPlot"), width = 12, height = 400
        )
      ),
      tabItem(
        tabName = "AccGraph",
        box(
          selectInput("dropdown1", label = "Choose Sessions", choices = unique(data$Lap))
        ),
        box(
          plotOutput("gearVSspeed"), width = 12, height = 300
        )
      ),
      tabItem(
        tabName = "LiveAnalysis",
        fluidRow(
          box(
            title = "Speedometer",
            width = 12,
            height = 300,
            plotlyOutput("speedometer")
          )
        ),
        fluidRow(
          box(
            downloadButton("downloadData", "Download Speed Data"),
            width = 12
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  selected_data1  <- reactive({
    laps_selected <- input$dropdown1
    data %>%
      filter(Lap %in% laps_selected)
  })
  
  selected_data2 <- reactive({
    laps_selected <- input$dropdown2
    data %>%
      filter(Lap %in% laps_selected)
  })
  
  output$mapPlot <- renderPlot({
    ggplot(selected_data2(), aes(x = NGPSLongitude, y = NGPSLatitude, col = factor(Lap), size = abs(gLat))) +
      geom_point() +
      facet_wrap(~Lap, scales = "free") +
      labs(title = "Side-by-Side Lap Comparison")
  })
  output$gearVSspeed <- renderPlot({
    ggplot(selected_data1(),aes(x=sLap,y=vCar,col=NGear))+geom_line()
  })
  # Reactive values for storing speed data
  speed <- reactiveVal(0)
  
  # Function to update speed every second
  observe({
    invalidateLater(1000, session)
    
    # Simulate speed update (replace with your actual data)
    newSpeed <- runif(1, min = 0, max = 100)
    
    # Update the speed value
    speed(newSpeed)
  })
  
  # Output for the speedometer
  output$speedometer <- renderPlotly({
    # Create a gauge-like plot with plotly
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = speed(),
      title = "Speed",
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, 50), color = "green"),
          list(range = c(50, 80), color = "yellow"),
          list(range = c(80, 100), color = "red")
        )
      )
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
