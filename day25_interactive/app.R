
library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("Precipitation in Ghana, Burkina Faso and Niger"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("Please select one or several points to see detailed information."),
      plotOutput("country_map", 
                 click = "plot1_click",
                 brush = "plot1_brush",
                 height="400px"
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("differences",height = "500px")
    )
  ),
  
  p("CPC Global Unified Precipitation data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their website at https://psl.noaa.gov/data/gridded/data.cpc.globalprecip.html"),
  p("Richard Vogg, 25.11.2021")
)


# Load data

source("Helpers.R")

library(ggtext)

prec <- data.table::fread("data/prec_summary.csv")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  vals <- reactiveValues(lat=NULL,
                         lon=NULL,
                         map=make_map(prec),
                         diff = NULL)
  
  output$country_map <- renderPlot({
    return(vals$map)
  })
  
  output$differences <- renderPlot({
    return(vals$diff)
  })
  
  observeEvent(input$plot1_click, {
    point <- nearPoints(prec %>% distinct(x,y), input$plot1_click, addDist = FALSE)
    if(length(point[[1]])==0) {} #happens when no point is selected
    else {
      vals$lon <- point[[1]]
      vals$lat <- point[[2]]
      vals$map <- plot_selected_point_in_map(prec,vals$lon,vals$lat)
      vals$diff <- plot_30y_avg_vs_today(prec,vals$lon,vals$lat)
    }
    
  })
  
  observeEvent(input$plot1_brush, {
    point <- brushedPoints(prec %>% distinct(x,y), input$plot1_brush)
    if(length(point[[1]])==0) {} #happens when no point is selected
    else {
      vals$lon <- point[[1]]
      vals$lat <- point[[2]]
      vals$map <- plot_selected_point_in_map(prec,vals$lon,vals$lat)
      vals$diff <- plot_30y_avg_vs_today(prec,vals$lon,vals$lat)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
