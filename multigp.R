# Drone Race Data
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Load and Trasnsform Data
race_results <- read.csv(file = "multigp_course_times.csv", header = T, stringsAsFactors = F)
race_results$Date.Recorded <- mdy(race_results$Date.Recorded)
race_results$Pilot_link <- paste0("https://www.multigp.com/pilots/view/?pilot=", 
                                  race_results$Pilot.Handle) #create link to pilot site
race_results$Pilot_link <- str_replace_all(race_results$Pilot_link, pattern = " ", replacement = "") #remove spaces from url link
race_results$Course <- as.factor(race_results$Course)
race_results$Chapter <- as.factor(race_results$Chapter)

# summary(race_results)
race_results <- race_results %>%
        filter(Date.Recorded > ymd("2015-1-1")) #remove misrecorded data

# Subset
summary(race_results$Course)

bessel_results <- race_results %>% filter(Course == "Bessel Run")
bessel_run <- race_results %>% filter(Course == "Bessel Run")
fury <- race_results %>% filter(Course == "Fury")
high_voltage <- race_results %>% filter(Course == "High Voltage")
# Order results by fastest

# Test plots
high_voltage_plot <- plot_ly(high_voltage, x = ~Date.Recorded, y = ~Time,
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
        add_trace(name = paste0("Mean Race Time ", round(mean(high_voltage$Time), digits = 2), " secs"), 
                  y = mean(high_voltage$Time), mode = "lines",
                  line = list(color = "red", opacity = 0.2),
                  marker = list(opacity = 0)
        ) %>%
        add_annotations(
                text = paste0("Mean Race Time: ", 
                              round(mean(high_voltage$Time), 2), 
                              " secs"),
                x = mean(high_voltage$Date.Recorded),
                y = mean(high_voltage$Time),
                yanchor = "bottom",
                showarrow = FALSE,
                font = list(color = "red")
        ) %>%
        layout(title = "High Voltage Race Results",
               margin = list(l = 100),
               hoverlabel = list(font = list(color = "blue"),
                                 bgcolor = "#f6f6f6",
                                 bordercolor = "white"),
               xaxis = list(title = "Recorded Date"),
               showlegend = FALSE
        )
high_voltage_plot



fury_plot <- plot_ly(fury, x = ~Date.Recorded, y = ~Time,
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
        add_trace(name = paste0("Mean Race Time ", round(mean(fury$Time), digits = 2), " secs"), 
                  y = mean(fury$Time), mode = "lines",
                  line = list(color = "red", opacity = 0.2),
                  marker = list(opacity = 0)
        ) %>%
        add_annotations(
                text = paste0("Mean Race Time: ", 
                              round(mean(fury$Time), 2), 
                              " secs"),
                x = mean(fury$Date.Recorded),
                y = mean(fury$Time),
                yanchor = "bottom",
                showarrow = FALSE,
                font = list(color = "red")
        ) %>%
        layout(title = "Fury Race Results",
               margin = list(l = 100),
               hoverlabel = list(font = list(color = "blue"),
                                 bgcolor = "#f6f6f6",
                                 bordercolor = "white"),
               xaxis = list(title = "Recorded Date"),
               showlegend = FALSE
        )
fury_plot


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


# race_plot <- ggplot(data = race_results, aes(x = Time, y = Chapter, color = Course)) +
#         geom_point(alpha = 0.25)
# race_plot
# 
# bessel_plot <- ggplot(data = bessel_run, aes(x = Time, y = Chapter, color = Pilot.Handle)) +
#         geom_point(alpha = 0.25)
# bessel_plot

# str(bessel_run)
# summary(bessel_run$Time)
# sd(bessel_run$Time)
# sum(bessel_run$Time > 3*sd(bessel_run$Time))
# plot(bessel_run$Time)
# hist(bessel_run$Time)
