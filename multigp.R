# Drone Race Data
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggExtra)

# Load and Trasnsform Data
race_results <- read.csv(file = "multigp_course_times.csv", header = T, stringsAsFactors = F)
race_results$Date.Recorded <- mdy(race_results$Date.Recorded)
race_results$Pilot_link <- paste0("https://www.multigp.com/pilots/view/?pilot=", 
                                  race_results$Pilot.Handle) #create link to pilot site
race_results$Pilot_link <- str_replace_all(race_results$Pilot_link, pattern = " ", replacement = "") #remove spaces from url link
race_results$Course <- as.factor(race_results$Course)
race_results$Chapter <- as.factor(race_results$Chapter)

summary(race_results)


# Subset
bessel_results <- race_results %>% filter(Course == "Bessel Run")
x <- bessel_results %>% 
        group_by(Chapter) %>%
        summarise(., quickest_time_in_chp = min(Time))

y <- left_join(bessel_results, x, by = "Chapter")

# Order results by fastest
y$Chapter <- factor(y$Chapter, levels = x$Chapter[order(x$quickest_time_in_chp)])


# Plot Data
race_plot <- ggplot(data = race_results, aes(x = Time, y = Chapter, color = Course)) +
        geom_point(alpha = 0.25)
race_plot

bessel_plot <- ggplot(data = y, aes(x = Time, y = Chapter, color = Date.Recorded)) +
        geom_vline(xintercept = min(y$Time), color = "red", alpha = 0.5, size = 1) +
        geom_point(alpha = 0.5, size =2) +
        scale_y_discrete(limits = rev(levels(y$Chapter))) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80),
                           limits = c(0, max(y$Time+5))) +
        theme_tufte() 
# theme(legend.position = "none")
bessel_plot
# ggMarginal(bessel_plot, type = "histogram", margins = "x")

ggplot(bessel_results, aes(Time)) +
        geom_density(fill = "red", alpha = 0.5) + 
        xlim(0, max(bessel_results$Time)+10)

ggplot(bessel_results, aes(x = Time, y = Pilot.Handle)) +
        geom_point(alpha = 0.5)
