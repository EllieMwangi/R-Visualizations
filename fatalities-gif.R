library(tidyverse)
library(animation)
library(tweenr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)

#Load data
fatalities <- read.csv('./data/Fatalities.csv', stringsAsFactors = FALSE)

#Manipulate date object to extract month and year
fatalities$ymd <- mdy_hms(fatalities$ï..Month)
fatalities$Year <- year(fatalities$ymd)
fatalities$Month <- month(fatalities$ymd, label = TRUE)

#Delete the unneccesary columns
fatalities <- fatalities[-c(1,3,4)]

str(fatalities)

fatalities <- fatalities %>% mutate(Year = factor(Year))
attach(fatalities)

#Compute y labels

max_fatality = max(No_of_fatalities)
max_fatality_limit = ceiling(max_fatality/50) * 50

y_breaks = seq(0, max_fatality_limit, 50)
y_labels = format(y_breaks, big.mark = ",")

# Set the x axis limits.
x_limits = rev(levels(Month))

data_list = list()

index = 1

for (year in sort(unique(Year))) {
  data_list[[index]] = fatalities %>% filter(Year == year)
  index = index + 1
}

tween_data = tween_states(data_list, 1, 2, "cubic-in-out", 120)

frames = sort(unique(tween_data$.frame))


saveGIF({
  for (frame in frames){
    frame_data = tween_data %>% filter(.frame == frame)
    
    #Compute graph title
    
    year = frame_data$Year[[1]]
    sum_fatalities = fatalities %>% filter(Year == year) %>% group_by(Year) %>% summarise(Sum = sum(No_of_fatalities))
    sum_fatalities_fmt = format(sum_fatalities$Sum[[1]], big.mark = ",")
    title = paste("Number of Fatalities", year, "Total of", sum_fatalities_fmt, "people")
    cat(title, "\n")
    
    p = ggplot(frame_data, aes(Month, No_of_fatalities, fill = Month)) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      scale_y_continuous(breaks = y_breaks,
                         expand = c(0, 0),
                         labels = y_labels,
                         limits = c(0, max_fatality_limit)) +
      scale_fill_brewer(palette = "Set3", guide = FALSE) +
      scale_x_discrete(limits = x_limits) +
      ggtitle(title) +
      xlab("") +
      ylab("Number of fatalities") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))
    print(p)
  }
}, movie.name = "fatalities.gif", interval = 0.01, ani.width = 720, ani.height = 480)