# Test Dashabord light framework
library('shiny')
library('shinydashboard')
library('ggplot2')
library('ggthemes')

ui <- dashboardPage(
        dashboardHeader(title = "Drone Racing"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "Bessel", 
                                 tabName = "bessel"
                                 )
                        )
                ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "bessel",
                                h2("Bessel Run Times"),
                                plotOutput("bessel_plot", width = "auto", height = "600px" )
                                )
                        )
                )
)
        

server <- function(input,output){
        output$bessel_plot <- renderPlot({
                data <- bessel_results # cleaned bessel results
                bessel_plot <- ggplot(data = y, aes(x = Time, y = Chapter, color = Date.Recorded)) +
                        geom_vline(xintercept = min(y$Time), 
                                   color = "red", 
                                   alpha = 0.5, 
                                   size = 1) +
                        geom_point(alpha = 0.5, size =2) +
                        scale_y_discrete(limits = rev(levels(y$Chapter))) +
                        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80),
                                           limits = c(0, max(y$Time+5))) +
                        theme_tufte()
                print(bessel_plot)
        })
}


shinyApp(ui, server) #preview dashboard
