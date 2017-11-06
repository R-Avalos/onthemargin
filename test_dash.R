# Test Dashabord light framework
library(shiny)
library(DT)
library(shinydashboard)
library(gridExtra)

## Functions
# googleSearchLink <- function(val) {
#         sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
# } # google search

# createLink <- function(link, reference) {
#         paste0('<a href=', link, ">", reference, "</a>")
# } # pilot link
# apply(test_df, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))
# test_df$url_link <- apply(test_df, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))
# 
# bessel_results$Pilot_link <- apply(bessel_results, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle']))

### Data


# Density Plot Table Colors
cols <- matrix("black", nrow(fury_top5), ncol = ncol(fury_top5))
cols[6, 2:3] <- "dodger blue"

## Dashboard

ui <- dashboardPage(
        skin = "green",
        dashboardHeader(title = "MultiGP Drone Racing"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "MultiGP League Overview",
                                 tabName= "allcourses"),
                        menuItem(text = "Chapters", 
                                 tabName = "chapters"
                                 ),
                        menuItem(text = "Pilots",
                                 tabName = "pilots"
                                 ),
                        menuItem(text = "Course Time Distribution",
                                 tabName = "course_t_distribution"
                                 ),
                        menuItem(text = "Data",
                                 tabName = "data_table")
                        )
                ),
        dashboardBody(
                tabItems(
#### All Courses Tab ####
                        tabItem(tabName = "allcourses",
                                fluidRow(
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(summary_df$Active_Pilots,
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Active Pilots</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(summary_df$Active_Chapter_Count,
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Active Chapters</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(summary_df$Count_Races,
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Races</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(summary_df$Course_Count,
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Courses</span>")
                                                )
                                        )
                                ),
                                ### Summary Row
                                fluidRow(
                                        column(
                                                width = 6,
                                                HTML(paste0("<p align = 'left'><span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(active_pilots$Pilot.Handle[1],
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Most Active Pilot (",
                                                            active_pilots$Races[1],
                                                            " races)</span></p>")
                                                )
                                        ),
                                        column(
                                                width = 6,
                                                HTML(paste0("<p align = 'left'><span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(active_chapters$Chapter[1],
                                                                                                                                        big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Most Active Chapter (",
                                                            active_chapters$Races[1],
                                                            " races)</span></p>")
                                                )
                                        )
                                ),
                                fluidRow(
                                        plotlyOutput(outputId = "allcourse_plot")
                                )
                        ),
#### Chapters Tab ####
                        tabItem(tabName = "chapters",
                                fluidRow(
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(nrow(chapter_summary_df), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Active</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(chapter_summary_df$Active_Pilots),2), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg Active Pilots </span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(chapter_summary_df$Races),2), big.mark = ","),
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg Race Count</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(chapter_summary_df$duration_active_days),2), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg Chapter Age</span>")
                                                )
                                        )
                                ),
                                plotlyOutput(outputId = "chp_count"),
                                selectInput(inputId = "chapter_select", 
                                            label = "Select Chapter", 
                                            choices = chapter_choice, 
                                            selected = active_chapters$Chapter[active_chapters$Races == max(active_chapters$Races)]),
                                plotlyOutput(outputId = "chapter_plot"),
                                DT::dataTableOutput(outputId = "chp_table")
                                ),
##### Pilots Tab ####
                                tabItem(tabName = "pilots",
                                fluidRow(
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(nrow(pilot_summary_df), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Active</span>")
                                                )
                                        ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(pilot_summary_df$Races),2), big.mark = ","),
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg Race Count</span>"
                                                            )
                                                     )
                                                ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(pilot_summary_df$duration_active_days),2), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg Days Active</span>")
                                                     )
                                                ),
                                        column(
                                                width = 3,
                                                HTML(paste0("<span style='font-family:Gill Sans; font-size:20px;'> <strong>", prettyNum(round(mean(pilot_count_df$count_start),2), big.mark = ","), 
                                                            "</strong></span>",
                                                            "<span style='font-family:Gill Sans; font-size:12px; color:grey;'>", 
                                                            " Avg New Pilots/Day</span>")
                                                )
                                        )
                                ),
                                plotlyOutput(outputId = "pilot_count"),
                                DT::dataTableOutput(outputId = "pilot_table")
                        ),
##### Course Time Distribution Tab ####

                        tabItem(tabName = "course_t_distribution",
                                selectInput(inputId = "course_top5", 
                                            label = "Select Course", 
                                            choices = unique(top5_results$Course), 
                                            selected = "Utt1"),
                                plotOutput("test")
                                ),
##### All Data Tab ####
                        tabItem(tabName = "data_table",
                                h2("All Race Results"),
                                dataTableOutput(outputId = "course_select_table")
                                )
                )
        )
)



server <- function(input,output){
        output$allcourse_plot <- renderPlotly({
                plot_ly(race_results, x = ~Date.Recorded, y = ~Time, color = ~Course,
                        opacity = 0.75,
                        colors = brewer.pal(6, "Dark2"),
                        type = "scatter",
                        mode = "markers",
                        hoverinfo = 'text',
                        text = ~paste0("<span style='color:grey'>Pilot Handle </span><b>", 
                                       Pilot.Handle, 
                                       "</b></br>",
                                       "</br>",
                                       "<span style='color:grey'>Chapter </span>", 
                                       Chapter,
                                       "</br><span style='color:grey'>Course </span>",
                                       Course,
                                       "</br><span style='color:grey'>Time </span>", 
                                       Time, 
                                       " secs")
                ) %>%
                        layout(title = "",
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               margin = list(r = 20),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               xaxis = list(showgrid = FALSE,
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            range = c(min(race_results$Date.Recorded)-30,
                                                      max(race_results$Date.Recorded)
                                                      ),
                                            autorange = FALSE,
                                            tickfont = list(family = "serif", size = 10), 
                                            ticks = "outside"
                               ),
                               yaxis = list(showgrid = FALSE,
                                            range = c(0, max(race_results$Time)+5),
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            tickvalues = summary(race_results$Time),
                                            ticksuffix = " secs",
                                            ticktext = round(summary(race_results$Time), 1),
                                            tickfont = list(family = "serif", size = 10), 
                                            ticks = "outside",
                                            zeroline = FALSE,
                                            zerolinecolor = toRGB("light grey")
                               ),
                               annotations = list(
                                       list(xref = "x", yref = "y", 
                                            x = ymd("2016-2-15"),
                                            y = max(race_results$Time)-5,
                                            text = "<b>MultiGP Drone Racing </b><br> Individual Pilot Times <br> by Course",
                                            showarrow = FALSE,
                                            align = "left")
                               )
                        )
                
        })

#### Data Table #### 
        output$course_select_table <- DT::renderDataTable({race_results_sub},
                                                          rownames = FALSE,
                                                          escape = FALSE)
        
#### Chapter Tab ####
        output$chp_count <- renderPlotly({
                plot_ly(chapter_count_df) %>%
                        add_trace(x = ~date, y = ~cumulative, 
                                  type = "scatter",
                                  mode = "lines",
                                  line = list(color = "red"),
                                  hoverinfo = 'text',
                                  text = ~paste0("<span style='color:grey'>Count Active Chapters </span><b>",
                                                 cumulative,
                                                 "</b></br>",
                                                 "</br>",
                                                 "<span style='color:grey'>Date </span>",
                                                 date
                                  )
                        ) %>%
                        add_trace(x = ~date, y = ~count_start, name = "Newly Active Chapters",
                                  type = "bar",
                                  marker = list(color = "black"),
                                  text = ~paste0("<span style='color:grey'>Newly Active Chapters </span><b>",
                                                 count_start,
                                                 "</b></br>"
                                  )
                        ) %>%
                        layout(title = "",
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               margin = list(r = 20),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               showlegend = FALSE,
                               xaxis = list(showgrid = FALSE,
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            autorange = TRUE,
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside"
                               ),
                               yaxis = list(showgrid = FALSE,
                                            range = c(0, max(chapter_count_df$cumulative)+5),
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside",
                                            zeroline = FALSE
                               ),
                               annotations = list(
                                       list(xref = "x", yref = "y",
                                            x = ymd("2016-2-15"),
                                            y = max(chapter_count_df$cumulative)-5,
                                            text = "<b>MultiGP Drone Racing </b><br> Running Total, Active <span style='color:red;'><b>Chapters</b></span>",
                                            showarrow = FALSE,
                                            align = "left")
                               )
                        )
                })
        output$chp_table <- DT::renderDataTable({chapter_summary_df},
                                                rownames = FALSE,
                                                escape = FALSE,
                                                options = list(lengthMenu = c(5, 30, 50), 
                                                               pageLength = 5,
                                                               order = list(list(2, 'desc')))
                                                )
        ### Reactive Chapter Plot
        chapter_df <- reactive({
                race_results %>% filter(Chapter == input$chapter_select)
        })
        output$chapter_plot <- renderPlotly({
                
                chapter_data <- chapter_df()
                chap_plot <- plot_ly(chapter_data, x = ~Date.Recorded, color = I(~Course),
                                     opacity = 0.75,
                                     colors = brewer.pal(6, "Dark2"),
                                     hoverinfo = 'text',
                                     text = ~paste0("<span style='color:grey'>Pilot Handle </span><b>",
                                                    Pilot.Handle,
                                                    "</b></br>",
                                                    "</br><span style='color:grey'>Course </span>",
                                                    Course,
                                                    "</br><span style='color:grey'>Time </span>",
                                                    Time,
                                                    " secs")
                ) %>%
                        add_markers(y = ~Time) %>%
                        add_lines(y = ~fitted(loess(Time ~ as.numeric(Date.Recorded))),
                                  line = list(colors = "Set1"),
                                  opacity = 0.5,
                                  name = "Loess Smoother", showlegend = FALSE) %>%
                        layout(title = "",
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               margin = list(r = 20),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               xaxis = list(showgrid = FALSE,
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            range = c(min(race_results$Date.Recorded)-30,
                                                      max(race_results$Date.Recorded)
                                            ),
                                            autorange = FALSE,
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside"
                               ),
                               yaxis = list(showgrid = FALSE,
                                            range = c(0, max(race_results$Time)+5),
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            tickvalues = summary(race_results$Time),
                                            ticksuffix = " secs",
                                            ticktext = round(summary(race_results$Time), 1),
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside",
                                            zeroline = TRUE,
                                            zerolinecolor = toRGB("light grey")
                               ),
                               annotations = list(
                                       list(xref = "x", yref = "y",
                                            x = ymd("2016-2-15"),
                                            y = max(race_results$Time)-5,
                                            text = paste0("<b><span style='color:blue'>",
                                                          chapter_data$Chapter[1], 
                                                          "</span></b><br>",
                                                          "Individual Pilot Times by Course<br>",
                                                          length(unique(chapter_data$Pilot.Handle)),
                                                          " Active Pilots<br>"
                                            ),
                                            showarrow = FALSE,
                                            align = "left")
                               ),
                               shapes=list(type='line',
                                           x0= min(race_results$Date.Recorded-30),
                                           x1= max(race_results$Date.Recorded),
                                           y0= min(race_results$Time),
                                           y1= min(race_results$Time),
                                           line=list(dash='dot', width=1, color = "grey"))
                        )
                print(chap_plot)
                
        })
        
        
#### Pilots Tab ####
        output$pilot_count <- renderPlotly({
                plot_ly(pilot_count_df) %>%
                        add_trace(x = ~date, y = ~cumulative, 
                                  type = "scatter",
                                  mode = "lines",
                                  line = list(color = "blue"),
                                  hoverinfo = 'text',
                                  text = ~paste0("<span style='color:grey'>Count Active Pilots </span><b>",
                                                 cumulative,
                                                 "</b></br>",
                                                 "</br>",
                                                 "<span style='color:grey'>Date </span>",
                                                 date
                                  )
                        ) %>%
                        add_trace(x = ~date, y = ~count_start, name = "Newly Active Pilots",
                                  type = "bar",
                                  marker = list(color = "black"),
                                  text = ~paste0("<span style='color:grey'>Newly Active Pilots </span><b>",
                                                 count_start,
                                                 "</b></br>"
                                  )
                        ) %>%
                        layout(title = "",
                               paper_bgcolor = "transparent",
                               plot_bgcolor = "transparent",
                               margin = list(r = 20),
                               hoverlabel = list(font = list(color = "blue"),
                                                 bgcolor = "white",
                                                 bordercolor = "white"),
                               showlegend = FALSE,
                               xaxis = list(showgrid = FALSE,
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            autorange = TRUE,
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside"
                               ),
                               yaxis = list(showgrid = FALSE,
                                            range = c(0, max(pilot_count_df$cumulative)+100),
                                            title = "",
                                            tickmode = "array",
                                            type = "marker",
                                            tickfont = list(family = "serif", size = 10),
                                            ticks = "outside",
                                            zeroline = FALSE
                               ),
                               annotations = list(
                                       list(xref = "x", yref = "y",
                                            x = ymd("2016-2-15"),
                                            y = max(pilot_count_df$cumulative)-5,
                                            text = "<b>MultiGP Drone Racing </b><br> Running Total: Active <span style='color:blue;'><b>Pilots</b></span>",
                                            showarrow = FALSE,
                                            align = "left")
                               )
                        )
        })
        output$pilot_table <- DT::renderDataTable({pilot_summary_df},
                                                rownames = FALSE,
                                                escape = FALSE,
                                                options = list(lengthMenu = c(5, 30, 50), 
                                                               pageLength = 5,
                                                               order = list(list(2, 'desc')))
                                                )

        

#### Course Time Tab #### 
        ### Reactive Density Plot with Table Overlay
        top5_data <- reactive({
                top5_results %>% filter(Course == input$course_top5)
        })
        course_data <- reactive({
                race_results %>% filter (Course == input$course_top5)
        })
        
        output$test <- renderPlot({
                table_d <- top5_data()
                density_df <- course_data()
                table_df <- table_d[order(table_d$Time),]
                
                p <- ggplot(density_df, aes(x = Time)) +
                        stat_density(fill = "black", alpha = 0.2) +
                        expand_limits(y = 0) +
                        geom_vline(xintercept = mean(density_df$Time), color = "dodger blue") +
                        xlab("Time (Seconds)") +
                        scale_x_continuous(limits = c(0, max(race_results$Time))) +
                        scale_y_continuous(limits = c(0, .14)) +
                        annotation_custom(tableGrob(table_df[,1:3], 
                                                    rows = NULL, 
                                                    theme = ttheme_minimal(
                                                            core = list(fg_params = list(col = cols)),
                                                            colhead=list(fg_params=list(col="grey", fontface=4L))
                                                    )),  
                                          xmin = -Inf, xmax = Inf,
                                          ymin = -Inf, ymax = Inf) +
                        #xmin = 50, xmax = max(fury$Time)-10,
                        #ymin = 0.025, ymax = Inf) +
                        ggtitle(paste0(density_df$Course[1], " Race Results")) +
                        theme_tufte() +
                        theme(plot.title = element_text(hjust = 0.5))
                print(p)
                
        })
}

shinyApp(ui, server) #preview dashboard
