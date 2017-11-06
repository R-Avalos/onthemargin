# Drone Race Data
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(RColorBrewer)

# Create Functions ####
createLink <- function(link, reference) {
        paste0('<a href=', link, ">", reference, "</a>")
}
func_top5 <- function(race_data = race_results, course_name) {
        top5 <- race_data %>%
                filter(Rank < 6 & Course == course_name) %>%
                select(Rank, Pilot.Handle, Time, Course)
        race_avg <- race_data %>%
                filter(Course == course_name) %>%
                summarise(Time = round(mean(Time), 2))
        race_avg$Rank <- ""
        race_avg$Pilot.Handle <- "AVG"
        race_avg$Course <- top5$Course[1]
        top5 <- rbind(top5, race_avg) #row bind tables
        top5 <- top5 %>% rename(`Pilot` = Pilot.Handle)
        print(top5)
        return(top5)
} # Function to return top 5 and average into table, order fast-slow with mean avg at end.


# Load and Trasnsform Data ####
race_results <- read.csv(file = "multigp_course_times.csv", header = T, stringsAsFactors = F)
race_results$Date.Recorded <- mdy(race_results$Date.Recorded)
race_results$Pilot_link <- paste0("https://www.multigp.com/pilots/view/?pilot=", 
                                  race_results$Pilot.Handle) #create link to pilot site
race_results$Pilot_link <- str_replace_all(race_results$Pilot_link, pattern = " ", replacement = "") #remove spaces from url link
race_results$Course <- as.factor(race_results$Course)
race_results$Chapter <- as.factor(race_results$Chapter)
race_results <- race_results %>%
        filter(Date.Recorded > ymd("2015-1-1")) #remove misrecorded data
race_results$year <- year(race_results$Date.Recorded)
race_results$month <- month(race_results$Date.Recorded)
race_results$pilot_url <- apply(race_results, MARGIN = 1, FUN = function(x) createLink(x['Pilot_link'], x['Pilot.Handle'])) #Create clickable link

race_results_sub <- race_results %>% select(Rank, Pilot.Handle, Full.Name, Chapter, Time, Date.Recorded, Course, pilot_url)


# Summary data frames ####
summary_df <- race_results %>%
        summarise(Course_Count = n_distinct(Course),
                  Active_Chapter_Count = n_distinct(Chapter),
                  Active_Pilots = n_distinct(Pilot.Handle),
                  Count_Races = n()
                  )
active_pilots <- race_results %>%
        group_by(Pilot.Handle) %>%
        summarise(Races = n()) %>%
        arrange(desc(Races))

active_chapters <- race_results %>%
        group_by(Chapter) %>%
        summarise(Races = n()) %>%
        arrange(desc(Races))

# Order results by fastest
### Top 5 Pilots by COurse, Data Frame
bessel_top5 <- func_top5(race_data = race_results, course_name = "Bessel Run")
fury_top5 <- func_top5(race_data = race_results, course_name = "Fury")
high_top5 <- func_top5(race_data = race_results, course_name = "High Voltage")
nautilus_top5 <- func_top5(race_data = race_results, course_name = "Nautilus")
tsunami_top5 <- func_top5(race_data = race_results, course_name = "Tsunami")
utt1_top5 <- func_top5(race_data = race_results, course_name = "UTT1")

# Density Plot Table Colors
cols <- matrix("black", nrow(fury_top5), ncol = ncol(fury_top5))
cols[6, 2:3] <- "dodger blue"
top5_results <- rbind(bessel_top5, fury_top5, high_top5, nautilus_top5, tsunami_top5, utt1_top5)
remove(bessel_top5, high_top5, nautilus_top5, utt1_top5)

### Chapter Data ####

# Count active chapters
chapter_summary_df <- race_results %>%
        group_by(Chapter) %>%
        summarize(Active_Pilots = length(unique(Pilot.Handle)),
                  Races = length(Time),
                  Unique_Tracks_Raced = length(unique(as.character(Course))),
                  date = min(Date.Recorded),
                  Last_Race_Date = max(Date.Recorded)
        )
chapter_summary_df$duration_active_days <- as.numeric(chapter_summary_df$Last_Race_Date-chapter_summary_df$date)+1

# DF of Chapter counts by their first active date
df_time <- data.frame(date = seq(min(race_results$Date.Recorded), 
                                 max(race_results$Date.Recorded), 
                                 by="day"))

chapter_count_df <- chapter_summary_df %>%
        group_by(date) %>%
        summarize(count_start = n())
chapter_count_df <- merge(x = df_time, y = chapter_count_df, by = "date", all.x =T)
chapter_count_df[is.na(chapter_count_df)] <- 0
chapter_count_df$cumulative <- cumsum(chapter_count_df$count_start)

# Selection string for plot input
chapter_choice <- active_chapters[with(active_chapters, order(Chapter)),]
chapter_choice <- chapter_choice$Chapter       

#### Pilot Data Frame ####
pilot_summary_df <- race_results %>%
        group_by(Pilot.Handle, Full.Name) %>%
        summarize(Races = length(Time),
                  Unique_Tracks_Raced = length(unique(as.character(Course))),
                  First_Race_Date = min(Date.Recorded),
                  Last_Race_Date = max(Date.Recorded),
                  URL = pilot_url[1]
        )
pilot_summary_df$duration_active_days <- as.numeric(pilot_summary_df$Last_Race_Date-pilot_summary_df$First_Race_Date)+1


pilot_count_df <- pilot_summary_df %>%
        group_by(First_Race_Date) %>%
        summarize(count_start = n())
colnames(pilot_count_df)[1] <- "date"
pilot_count_df <- merge(x = df_time, y = pilot_count_df, by = "date", all.x =T)
pilot_count_df[is.na(pilot_count_df)] <- 0
pilot_count_df$cumulative <- cumsum(pilot_count_df$count_start)