# Shiny Dashboard App
library('shinydashboard')
library('shiny')

ui <- dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody()
)

server <- function(input,output){
}

shinyApp(ui, server) #preview dashboard