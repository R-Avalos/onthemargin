# Drone Race Data
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggthemes)
library(plotly)

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
race_results$year <- year(race_results$Date.Recorded)
race_results$month <- month(race_results$Date.Recorded)


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

### Top 5 Pilots by COurse, Data Frame
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
}
bessel_top5 <- func_top5(race_data = race_results, course_name = "Bessel Run")
fury_top5 <- func_top5(race_data = race_results, course_name = "Fury")
high_top5 <- func_top5(race_data = race_results, course_name = "High Voltage")
nautilus_top5 <- func_top5(race_data = race_results, course_name = "Nautilus")
tsunami_top5 <- func_top5(race_data = race_results, course_name = "Tsunami")
utt1_top5 <- func_top5(race_data = race_results, course_name = "UTT1")
top5_results <- rbind(bessel_top5, fury_top5, high_top5, nautilus_top5, tsunami_top5, utt1_top5)


### Chapter Data ###
###################

# Count active chapters
active_chapters <- unique(race_results$Chapter)
length(active_chapters)

chapter_df <- race_results %>%
        group_by(Chapter) %>%
        summarize(Active_Pilots = length(unique(Pilot.Handle)),
                  Races = length(Time),
                  Unique_Tracks_Raced = length(unique(as.character(Course))),
                  First_Race_Date = min(Date.Recorded),
                  Last_Race_Date = max(Date.Recorded)
        )
head(chapter_df)
summary(chapter_df)

chapter_race_df <- race_results %>%
        group_by(Chapter, year, month, Course) %>%
        summarize(Races = length(Time),
                  Min_Time = min(Time),
                  Max_Time = max(Time),
                  Mean_Time = mean(Time),
                  Median_Time = median(Time),
                  SD_Time = sd(Time))

chapter_race_df$date <- ymd(paste0(chapter_race_df$year, "-",
                                   chapter_race_df$month, "-",
                                   "1")
                            )

# Add filler dates
df_time <- data.frame(date = seq(min(race_results$Date.Recorded), 
                                max(race_results$Date.Recorded), 
                                by="day"))
summary(df_time)
chapter_race_df <- merge(x=df_time, y=chapter_race_df, by="date", all.x=T)

head(chapter_race_df)
summary(chapter_race_df)

#### Test plots  ###
###################

## Chapter Drill Down Plot, Single chapter's Mean Time by Course over time, with SD band along with min and max dashed lines. Select Courses to display...

# Pre-filtered to #TeamBaylands
teambaylands_df <- chapter_race_df %>%
        filter(Chapter == "#TeamBaylands")
summary(teambaylands_df)

plot_ly(data = teambaylands_df, x = ~date, y = ~Mean_Time, color = ~Course,
        type = "scatter", mode = "lines + markers") %>% 
        add_lines(x = ~date, y = ~Mean_Time-SD_Time,
                  line = list(dash = "5px"),
                  opacity = 0.25,
                  hoverinfo = "Text", 
                  text =  "1 Std Dev Below Mean Time"
                  ) %>%   # Add SD dash line below mean time
        add_lines(x = ~date, y = ~Mean_Time+SD_Time,
                  line = list(dash = "5px"),
                  opacity = 0.25,
                  hoverinfo = "Text", 
                  text =  "1 Std Dev Above Mean Time"
                  ) %>% # Add SD dash line above mean time
        add_markers(x = ~date, y = ~Mean_Time-SD_Time,
                    opacity = 0.5,
                    marker = list(symbol = "square")
                    ) %>%
        add_markers(x = ~date, y = ~Mean_Time+SD_Time,
                    opacity = 0.5,
                    marker = list(symbol = "square")
        ) %>%
        layout(xaxis = list(title = "", showgrid = F,
                            tickfont = list(family = "serif", size = 10), ticks = "outside"),
               yaxis = list(title = "", showgrid = F, tickmode = "array",  
                            type = "linear", 
                            ticksuffix  = " secs",
                            tickfont = list(family = "serif", size = 10), ticks = "outside")
        )

p <- ggplot(fury, aes(x = Time)) +
        stat_density(fill = "black", alpha = 0.2) +
        expand_limits(y = 0) +
        geom_vline(xintercept = mean(fury$Time), color = "dodger blue") +
        xlab("Time (Seconds)") +
        annotation_custom(tableGrob(fury_top5[,1:3], 
                                    rows = NULL, 
                                    theme = ttheme_minimal(
                                            core = list(fg_params = list(col = cols)),
                                            colhead=list(fg_params=list(col="grey", fontface=4L))
                                    )),  
                          xmin = -Inf, xmax = Inf,
                          ymin = -Inf, ymax = Inf) +
                          #xmin = 50, xmax = max(fury$Time)-10,
                          #ymin = 0.025, ymax = Inf) +
        ggtitle(paste0(fury$Course[1], " Race Results")) +
        theme_tufte() +
        theme(plot.title = element_text(hjust = 0.5))
p

#####
test_p <- ggplotly(p)   
test_p

fury_avg_text <- list(
        x = mean(fury$Time),
        y = 0.075,
        text = paste0("Avg Time ", round(mean(fury$Time), 2), " secs"),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 8,
        arrowsize = 0.25,
        arrowcolor = "#D3D3D3",
        xanchor = "left",
        ax = sd(fury$Time),
        ay = 0,
        font = list(color = "#D3D3D3")
)
test_p2 <- test_p %>%
        layout(annotations = fury_avg_text)
fury_top5
subplot(test_p2, fury_top5)
renderTable(fury_top5, 
            hover = TRUE,
            spacing = "xs")
#


fury_fit <- density(fury$Time)
plot_ly(x = fury$Time, type = "histogram", name = "Fury", alpha = 0, color = "red") %>%
        add_trace(x = fury_fit$x, y = fury_fit$y, 
                  type = "scatter", mode = "lines", 
                  fill = "tozeroy", yaxis = "y2") %>%
        layout(yaxis2 = list(overlaying = "y", side = "right"))
        


#### Create Plot Function for courses

course_plotfunc <- function(course_df){
        plot <- plot_ly(data = course_df, x = ~Date.Recorded, y = ~Time,
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
                add_trace(name = paste0("Mean Race Time ", round(mean(course_df$Time), digits = 2), " secs"), 
                          y = mean(course_df$Time), mode = "lines",
                          line = list(color = "red", opacity = 0.2),
                          marker = list(opacity = 0)
                ) %>%
                add_annotations(
                        text = paste0("Mean Race Time: ", 
                                      round(mean(course_df$Time), 2), 
                                      " secs"),
                        x = mean(course_df$Date.Recorded),
                        y = mean(course_df$Time),
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(color = "red")
                ) %>%
                layout(title = paste0(course_df$Course[1], " Race Results"),
                       margin = list(l = 100),
                       hoverlabel = list(font = list(color = "blue"),
                                         bgcolor = "#f6f6f6",
                                         bordercolor = "white"),
                       xaxis = list(title = "Recorded Date"),
                       showlegend = FALSE
                )
        return(plot)
}

bessel_plot <- course_plotfunc(bessel_results)
bessel_plot


