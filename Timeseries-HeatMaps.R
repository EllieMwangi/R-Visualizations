#Load Packages
library(plyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(gganimate)

#Load data
df= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/ggmap/i2Sample.csv', stringsAsFactors = FALSE)

incidents <- df

#Define colour scale 
col1 = "#FFEE58" #Lowest values
col2 = "#FF7043" #Highest Values

#View first few rows of the data frame
head(incidents)

#Attach incidents dataframe to R search path
attach(incidents)
str(incidents)

#Using lubridate, manipulate clearance date and extract year,month,day and hour data
incidents$ymd <-mdy_hms(Event.Clearance.Date)
incidents$month <- lubridate::month(incidents$ymd, label = TRUE)
incidents$year <- lubridate::year(incidents$ymd)
incidents$wday <- lubridate::wday(incidents$ymd, label = TRUE)
incidents$hour <- lubridate::hour(incidents$ymd)

attach(incidents)
head(incidents)

#Create dayHour frame that summarises total number of incidents per each hour/day
dayHour <- ddply(incidents, c("hour", "wday"), summarise, N = length(ymd))

# factor and relevel the day variable for better visualization
dayHour$wday <- factor(dayHour$wday, levels=rev(levels(dayHour$wday)))

#Omit any nulls in the data
dayHour <- na.omit(dayHour)

attach(dayHour)

# Plot heatmap
p_dayHour <- ggplot(dayHour, aes(hour, wday)) +
  geom_tile(aes(fill=N), size = 0.25 ,colour = "white", na.rm = T) +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total incidents")) +
  theme_bw() + theme_minimal() +
  labs(title= "Histogram of Seattle incidents by day of week and hour", x = "Incidents per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))

p_dayHour


#Animate the heatmap using gganimate transition_time and transition_states functions
p_dayHour + transition_time(hour) + shadow_mark()

p_dayHour + transition_states(states = hour, transition_length = 5, state_length = 0.5,
                                wrap = T) + shadow_mark()
                                
#Create yearMonth frame that summarises total number of incidents per each month/year
yearMonth <- ddply(incidents, c("year", "month"), summarise, N = length(ymd))

yearMonth$month <- factor(yearMonth$month, levels=rev(levels(yearMonth$month)))

yearMonth <- na.omit(yearMonth)
attach(yearMonth)

#Plot heatmap
p_yearMonth <- ggplot(yearMonth, aes(year, month)) +
  geom_tile(aes(fill=N), colour = "white", na.rm = T) +
  scale_fill_gradient(low = col1, high = col2) +
  guides(fill = guide_legend(title = "Total incidents")) +
  theme_bw() + theme_minimal() +
  labs(title= "Histogram of Seattle incidents by month and year", x = "Year", y = "Month") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))


#Animate heatmap
p_yearMonth + transition_states(states = year, transition_length = 2, state_length = 1.5,
                                wrap = FALSE) + shadow_mark()
  
p_yearMonth + transition_time(year)+ shadow_mark()
