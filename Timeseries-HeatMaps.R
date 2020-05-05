library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(gganimate)

df= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/i2Sample.csv', stringsAsFactors = FALSE)

incidents <- df

col1 = "#FFEE58"
col2 = "#FF7043" 

head(incidents)
attach(incidents)
str(incidents)

incidents$ymd <-mdy_hms(Event.Clearance.Date)
incidents$month <- lubridate::month(incidents$ymd, label = TRUE)
incidents$year <- lubridate::year(incidents$ymd)
incidents$wday <- lubridate::wday(incidents$ymd, label = TRUE)
incidents$hour <- lubridate::hour(incidents$ymd)

attach(incidents)
head(incidents)

dayHour <- ddply(incidents, c("hour", "wday"), summarise, N = length(ymd))

dayHour$wday <- factor(dayHour$wday, levels=rev(levels(dayHour$wday)))


dayHour <- na.omit(dayHour)

attach(dayHour)

p_dayHour <- ggplot(dayHour, aes(hour, wday)) +
  geom_tile(aes(fill=N), size = 0.25 ,colour = "white", na.rm = T) +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total incidents")) +
  theme_bw() + theme_minimal() +
  labs(title= "Histogram of Seattle incidents by day of week and hour", x = "Incidents per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p_dayHour

p_dayHour + transition_time(hour) + shadow_mark()

p_dayHour + transition_states(states = hour, transition_length = 5, state_length = 0.5,
                                wrap = T) + shadow_mark()
                                

yearMonth <- ddply(incidents, c("year", "month"), summarise, N = length(ymd))

yearMonth$month <- factor(yearMonth$month, levels=rev(levels(yearMonth$month)))

yearMonth <- na.omit(yearMonth)
attach(yearMonth)

p_yearMonth <- ggplot(yearMonth, aes(year, month)) +
  geom_tile(aes(fill=N), colour = "white", na.rm = T) +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total incidents")) +
  theme_bw() + theme_minimal() +
  labs(title= "Histogram of Seattle incidents by month and year", x = "Year", y = "Month") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


#p_yearMonth + transition_states(states = year, transition_length = 2, state_length = 1.5,
                                #wrap = FALSE) + shadow_mark()
  
#p_yearMonth + transition_time(year)+ shadow_mark()
