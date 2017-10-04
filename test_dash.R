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
                        menuItem(text = "All Courses",
                                 tabName= "allcourses"),
                        menuItem(text = "Bessel Run", 
                                 tabName = "bessel"
                                 ),
                        menuItem(text = "Bessel Table",
                                 tabName = "bessel_table")
                        )
                ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "allcourses",
                                h2("MultiGP Recorded Race Times by Course"),
                                plotlyOutput(outputId = "allcourse_plot")
                                ),
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
        output$allcourse_plot <- renderPlotly({
                plot_ly(race_results, x = ~Date.Recorded, y = ~Time, color = ~Course,
                        alpha = 0.75, 
                        type = "scatter",
                        mode = "markers",
                        hoverinfo = 'text',
                        text = ~paste0("<span style='color:grey'>Pilot Handle </span><b>", 
                                       Pilot.Handle, 
                                       "</b></br>",
                                       "</br>",
                                       "<span style='color:grey'>Chapter </span>", 
                                       Chapter,
                                       "</br><span style='color:grey'> Course </span>",
                                       Course,
                                       "</br><span style='color:grey'> Time </span>", 
                                       Time, 
                                       " secs")
                ) %>%
                        layout(title = "MultiGP Race Results",
                               margin = list(l = 100),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               xaxis = list(title = "Recorded Date")
                        )
                
        })
        
}

shinyApp(ui, server) #preview dashboard
