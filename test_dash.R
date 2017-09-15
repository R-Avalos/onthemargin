# Test Dashabord light framework
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)

ui <- dashboardPage(
        skin = "green",
        dashboardHeader(title = "MultiGP Drone Racing"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "Bessel Run", 
                                 tabName = "bessel"
                                 )
                        )
                ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "bessel",
                                h2("Bessel Run, Race Times"),
                                fluidRow(
                                        
                                        radioButtons(
                                                inputId = "radio",
                                                label = "Selection Type:",
                                                choices = list(
                                                        "All",
                                                        "Manual Select"
                                                ),
                                                selected = "All"
                                        ),
                                        
                                        selectizeInput("select_chapter_input",
                                                    label = "Chapters",
                                                    choices = bessel_results$Chapter,
                                                    selected = bessel_results$Chapter[bessel_results$Time == min(bessel_results$Time)],
                                                    multiple = TRUE)
                                        
                                ),
                                plotOutput(
                                        "bessel_plot", 
                                        width = "auto", height = "600px",
                                        hover = hoverOpts(id = "bessel_hover")   
                                        )
                                )
                        )
                )
)

server <- function(input,output){
        output$bessel_plot <- renderPlot({
                plot_data <- reactive({
                        if(input$radio == "All"){
                                bessel_results
                        }
                        else {
                        bessel_results %>%
                                filter(Chapter == input$select_chapter_input)
                        }
                })
                        

                bessel_plot <- ggplot(data = plot_data(), 
                                      aes(x = Time, y = Chapter, color = Date.Recorded)) +
                        geom_vline(xintercept = min(plot_data()$Time), 
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
