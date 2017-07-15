# Shiny Dashboard App

# Potential data sources:
# github, cryptocurrency, f1 racing, SFMoMA





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
                        ),
                        fluidRow(
                                box(plotOutput("testPlot2", height = 250)),
                                box(
                                        title = "tesPlot2 Controls",
                                        sliderInput("slider2", "Count", 1, 500, 100)
                                )
                        )
                        )
        )
)

shinyApp(ui, server) #preview dashboard


server <- function(input,output){
        set.seed(42)
        histdata <- rnorm(500)
        histdata2 <- rnorm(1000)
        output$testPlot <- renderPlot(
                {
                data <- histdata[seq_len(input$slider)]
                hist(data)
        }
        )
        output$testPlot2 <- renderPlot(
                {
                        data <- histdata2[seq_len(input$slider2)]
                        hist(data)
                }
        )
}

