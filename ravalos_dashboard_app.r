# Shiny Dashboard App
library('shinydashboard')
library('shiny')

ui <- dashboardPage(
        dashboardHeader(title = "Randy Avalos"),
        dashboardSidebar(
                menuItem(text = "Github", 
                         tabName = "Github", 
                         icon = icon("github"),
                         badgeLabel = "Commit History",
                         badgeColor = "teal"
                         )
        ),
        dashboardBody(
                # Github tab
                tabItem(tabName = "Github",
                        fluidRow(
                                box(plotOutput("testPlot", height = 250)),
                                box(
                                        title = "testPlot Controls",
                                        sliderInput("slider", "Obs Count:", 1, 100, 50)
                                )
                        )
                        )
        )
)

shinyApp(ui, server) #preview dashboard


server <- function(input,output){
        set.seed(42)
        histdata <- rnorm(500)
        
        output$testPlot <- renderPlot(
                {
                data <- histdata[seq_len(input$slider)]
                hist(data)
        }
        )
}

