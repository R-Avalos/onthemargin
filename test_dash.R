# Test Dashabord light framework
library(shiny)
library(shinydashboard)
library(shiny)
library(dplyr)

## Functions
# googleSearchLink <- function(val) {
#         sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
# } # google search

createLink <- function(link, reference) {
        paste0('<a href=', link, ">", reference, "</a>")
} # pilot link

# bessel_results[1,]
# bessel_results$Pilot_link[1]
# bessel_results$Pilot.Handle[1]
# createLink(link = bessel_results$Pilot_link[1], reference = bessel_results$Pilot.Handle[1])


## Dashboard

ui <- dashboardPage(
        skin = "green",
        dashboardHeader(title = "MultiGP Drone Racing"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "Bessel Run", 
                                 tabName = "bessel"
                                 ),
                        menuItem(text = "Bessel Table",
                                 tabName = "bessel_table")
                        )
                ),
        dashboardBody(
                tabItems(
                        # tabItem(tabName = "bessel",
                        #         h2("Bessel Run, Race Times"),
                        #         plotOutput(
                        #                 "bessel_plot", 
                        #                 width = "auto", height = "600px")   
                        #                 )
                        #         ),
                        tabItem(tabName = "bessel_table",
                                h2("Bessel Run, Table of Results"),
                                
                                dataTableOutput(outputId = "bessel_table")
                                )
                )
        )
)



server <- function(input,output){
        output$bessel_table <- renderDataTable({bessel_results})
        
        # output$bessel_plot <- ggplot(data = bessel_results, 
        #                       aes(x = Time, y = Chapter, color = Date.Recorded)) +
        #         geom_vline(xintercept = min(bessel_results$Time), 
        #                    color = "red", 
        #                    alpha = 0.5, 
        #                    size = 1) +
        #         geom_point(alpha = 0.5, size =2) +
        #         scale_y_discrete(limits = rev(levels(bessel_results$Chapter))) +
        #         scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80)) +
        #         limits = c(0, max(bessel_results$Time+5)) +
        #         theme_tufte()
}

shinyApp(ui, server) #preview dashboard
