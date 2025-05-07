#Load packages
library(tidyverse)
library(httr2)
library(ggplot2)
library(shiny)
library(shinydashboard)

#Call API and pluck data
septaURL <- 'https://www3.septa.org/api/TransitViewAll'

response <- request(septaURL) %>% 
  req_perform()

performanceData<- response %>% 
  resp_body_json() %>% 
  pluck('routes', 1) %>%
  map_dfr(bind_rows) %>% 
  select(Route = route_id, Trip = trip, 
         Late = late, seatsAvailable = estimated_seat_availability)

#Drop inactive buses
performanceData<- subset(performanceData, Late< 900)

# Define UI

header <- dashboardHeader(title = "SEPTA Interactive Dashboard")

sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("System", tabName = "system", icon = icon("dashboard")),
      menuItem("Routes", icon = icon("dashboard"), tabName = "routes")
  )
)
  
body <- dashboardBody(
    tabItems(
      tabItem(tabName = "routes",
              h2(
                  fluidRow(
                  column(width = 12,
                    box(
                        title= "Routes", 
                        width = NULL, 
                        status = "primary",
                        div(dataTableOutput("routeTable")
                        , style = 'font-size:60%')
                  ),
                    box(
                       selectInput("v_select", 
                                   "Route", 
                      choices = unique(performanceData$Route))
        )
      )
    )
  )
),
      tabItem(tabName = "system",
              h2(
                fluidRow(
                  column(width = 12,
                         box(
                           title = "System", 
                           width = NULL, 
                           status = "primary",
                           plotOutput("latePlot")
      )
     )
    )
   )
  )
 )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {

    output$routeTable <- renderDataTable({
      performanceData %>% 
        filter(Route == input$v_select)
    })
    output$latePlot <- renderPlot({
      #Create lateness categorical variables
      performanceData$lateCat <- cut(performanceData$Late, 
                                     breaks = c(-Inf, 5, 15, Inf), 
                                     labels = c("less than 5", 
                                                "more than 5", 
                                                "more than 15"))
      #replace ggplot hist with shiny hist???
      performanceData %>% 
        ggplot() +
        geom_bar(aes(x=lateCat))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
