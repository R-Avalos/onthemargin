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

bessel_results[1,]
bessel_results$Pilot_link[1]
bessel_results$Pilot.Handle[1]
createLink(link = bessel_results$Pilot_link[1], reference = bessel_results$Pilot.Handle[1])

apply(test_df, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))
test_df$url_link <- apply(test_df, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))

bessel_results$Pilot_link <- apply(bessel_results, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))


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
                        tabItem(tabName = "bessel",
                                h2("Bessel Run, Race Times"),
                                plotlyOutput(outputId = "bessel_run_plot")
                                ),
                        tabItem(tabName = "bessel_table",
                                h2("Bessel Run, Table of Results"),
                                
                                dataTableOutput(outputId = "bessel_table")
                                )
                )
        )
)



server <- function(input,output){
        output$bessel_table <- renderDataTable({bessel_results}, escape = FALSE)
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
        output$bessel_run_plot <- renderPlotly({
                plot_ly(bessel_run, x = ~Date.Recorded, y = ~Time,
                        name = "Recorded Race Time",
                        type = "scatter",
                        mode = "markers",
                        hoverinfo = 'text',
                        text = ~paste0("<span style='color:grey'>Pilot Handle </span><b>", 
                                       Pilot.Handle, 
                                       "</b></br>",
                                       "</br>",
                                       "<span style='color:grey'>Chapter </span>", 
                                       Chapter,
                                       "</br><span style='color:grey'> Time </span>", 
                                       Time, 
                                       " secs"),
                        marker = list(color = 'rgb(0, 66, 37)', opacity = 0.4)
                ) %>%
                        add_trace(name = paste0("Mean Race Time ", round(mean(bessel_run$Time), digits = 2), " secs"), 
                                  y = mean(bessel_run$Time), mode = "lines",
                                  line = list(color = "red", opacity = 0.2),
                                  marker = list(opacity = 0)
                        ) %>%
                        add_annotations(
                                text = paste0("Mean Race Time: ", 
                                              round(mean(bessel_run$Time), 2), 
                                              " secs"),
                                x = mean(bessel_run$Date.Recorded),
                                y = mean(bessel_run$Time),
                                yanchor = "bottom",
                                showarrow = FALSE,
                                font = list(color = "red")
                        ) %>%
                        layout(title = "Bessel Run Race Results",
                               margin = list(l = 100),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "#f6f6f6",
                                                 bordercolor = "white"),
                               xaxis = list(title = "Recorded Date"),
                               showlegend = FALSE
                        )
                
        })
        
}

shinyApp(ui, server) #preview dashboard
