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


# Subset
bessel_results <- race_results %>% filter(Course == "Bessel Run")
# Order results by fastest

# Plot Data
plot_ly(race_results, x = ~Time, y = ~Chapter, color = ~Course, type = "scatter") %>%
        add_markers(alpha = 0.2) %>%
        layout(title = "MultiGP Race Results")


plot_ly(race_results, x = ~Time, y = ~Course, alpha = 0.1, type = "scatter") %>%
        layout(title = "MultiGP Race Results")


# race_plot <- ggplot(data = race_results, aes(x = Time, y = Chapter, color = Course)) +
#         geom_point(alpha = 0.25)
# race_plot
# 
# bessel_plot <- ggplot(data = bessel_results, aes(x = Time, y = Chapter, color = Pilot.Handle)) +
#         geom_point(alpha = 0.25)
# bessel_plot
