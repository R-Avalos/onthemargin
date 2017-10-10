# Drone Race Data
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)

# Load and Trasnsform Data
race_results <- read.csv(file = "multigp_course_times.csv", header = T, stringsAsFactors = F)
race_results$Date.Recorded <- mdy(race_results$Date.Recorded)
race_results$Pilot_link <- paste0("https://www.multigp.com/pilots/view/?pilot=", 
                                  race_results$Pilot.Handle) #create link to pilot site
race_results$Pilot_link <- str_replace_all(race_results$Pilot_link, pattern = " ", replacement = "") #remove spaces from url link
race_results$Course <- as.factor(race_results$Course)
race_results$Chapter <- as.factor(race_results$Chapter)
race_results <- race_results %>%
        filter(Date.Recorded > ymd("2015-1-1")) #remove misrecorded data

# Subset
summary(race_results$Course)

bessel_results <- race_results %>% filter(Course == "Bessel Run")
bessel_run <- race_results %>% filter(Course == "Bessel Run")
fury <- race_results %>% filter(Course == "Fury")
high_voltage <- race_results %>% filter(Course == "High Voltage")
nautilus <- race_results %>% filter(Course == "Nautilus")
tsunami <- race_results %>% filter(Course == "Tsunami")
utt1 <- race_results %>% filter(Course == "UTT1")


# Order results by fastest
#### Fury Top 5
# Function to return top 5 and average into table, order fast-slow with mean avg at end.
func_top5 <- function(race_data = race_results, course_name) {
        top5 <- race_data %>%
                filter(Rank < 6 & Course == course_name) %>%
                select(Rank, Pilot.Handle, Time)
        race_avg <- race_data %>%
                filter(Course == course_name) %>%
                summarise(Time = round(mean(Time), 2))
        race_avg$Rank <- ""
        race_avg$Pilot.Handle <- "AVG Course Time"
        top5 <- rbind(top5, race_avg) #row bind tables
        top5 <- top5 %>% rename(`Pilot Handle` = Pilot.Handle)
        print(top5)
        return(top5)
}

fury_top5 <- func_top5(race_data = race_results, course_name = "Fury")




# Test plots

p <- ggplot(fury, aes(x = Time)) +
        stat_density(fill = "red", alpha = 0.2) +
        expand_limits(y = 0) +
        geom_vline(xintercept = mean(fury$Time), alpha = 0.5) +
        xlab("Fury Course Times (Seconds)") +
        theme_tufte()
p <- p + annotate("text", x = mean(fury$Time)+12, y = 0.075, 
             label = paste0("Fury Mean Time \n", round(mean(fury$Time), 2), " secs"))
p
test_p <- ggplotly(p)   
test_p

fury_fit <- density(fury$Time)
plot_ly(x = fury$Time, type = "histogram", name = "Fury", alpha = 0, color = "red") %>%
        add_trace(x = fury_fit$x, y = fury_fit$y, 
                  type = "scatter", mode = "lines", 
                  fill = "tozeroy", yaxis = "y2") %>%
        layout(yaxis2 = list(overlaying = "y", side = "right"))
        



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
