#Code made by Jean Pierre Oliveros for TidyTuesday, for this week I am comparing the changes over the decade of doin business
#Set de Working Directory and load the packages
setwd("~/economia/Tidy Tuesdau/Tercera Entrega")
library(tidyverse)
library(readxl)
library(lubridate)
library(ggrepel)
library(scales)
library(ggtext)

#Load the data from The World Bank, but without the link, because it is a personalized query

Doing_business_world_bank <- read_excel("Doing_business - world_bank.xlsx")

#Clean the data

Countries = subset(Doing_business_world_bank, Economy == "Argentina"
                   |Economy == "New Zealand" |Economy == "Colombia"
                   |Economy == "Venezuela, RB" |Economy == "Ecuador"
                   |Economy == "Korea, Rep." |Economy == "Norway"
                   |Economy == "Mexico - Mexico City" |Economy == "Panama"
                   |Economy == "Peru")
Countries = Countries[ ,c(1:6)]
Countries$Economy[which(Countries$Economy == "Venezuela, RB")] = "Venezuela"
Countries$Economy[which(Countries$Economy == "Mexico - Mexico City")] = "Mexico"
Countries[45,3] = as.character(60)
Countries[is.na(Countries)] = ''
Countries[seq(from = 7, to = 106, by = 11), 6] = ''
Countries=unite(Countries, Scores, 
      `Ease of doing business score (DB17-20 methodology)`:`Ease of doing business score (DB10-14 methodology)`,
      sep = '')
Countries[ , 2] = rep(2020:2010, times = 10)
Countries$Year = paste(Countries$Year, "01-01", sep = '-')
Countries$Year = ymd(Countries$Year)
Countries$Scores = as.numeric(Countries$Scores)
Countries$`Ease of doing business rank (DB20)` = as.numeric(Countries$`Ease of doing business rank (DB20)`)
Countries$Scores = sprintf(Countries$Scores, fmt = '%#.2f')

#Separate the data
Scores = Countries
Scores$Rank = paste(Scores$Economy, Scores$`Ease of doing business rank (DB20)`, sep = '-')
Scores = Scores[ ,c(1,2,4,5)]

#Make the plot
ggplot(data = Scores, aes(x = Year, y = Scores, group = Economy))+
  geom_line(aes(color = Economy), size = 2)+
  geom_point(aes(color = Economy), size = 3)+
  geom_vline(aes(xintercept = as.Date("2015-01-01")), size = 1, color = "black", linetype = "dashed", alpha = 0.25)+
  geom_vline(aes(xintercept = as.Date("2014-01-01")), size = 1, color = "black", linetype = "dashed", alpha = 0.25)+
  geom_text_repel(data = Scores %>% filter(Year == "2010-01-01"), 
                  aes(label = Economy, color = Economy), 
                  fontface = "bold", 
                  size = 5,
                  hjust = "left",
                  nudge_x = -1000)+
  geom_text_repel(data = Scores %>% filter(Year == "2020-01-01"), 
                  aes(label = Rank, color = Economy), 
                  fontface = "bold", 
                  size = 5,
                  hjust = "right",
                  nudge_x = 1000000000000000000)+
  scale_x_date(breaks = "years", labels = date_format("%Y"))+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2015-01-01"), ymin = min(Scores$Scores), ymax = max(Scores$Scores), alpha = 0.15)+
  annotate("text", x = as.Date("2014-07-01"), y = 30, label = "Digitization
Process", alpha = 0.5)+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        plot.background = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.background = element_blank(),
        plot.caption = element_markdown(hjust = 0, size = 15),
        plot.subtitle = element_markdown(size = 15),
        plot.title = element_markdown(size = 18))+
  labs(title = "<strong><span>Impact of Digitization on Burocratic Process</span></strong>",
       subtitle = "Comparision of the <i><span style='color:#43a0f7'>Doing Business Scores</span></i> in Latin America and their ranks for 2020",
       caption = "<strong><span>Source:</span></strong> World Bank | <strong><span>Created by</span></strong> <i><span>Jean Pierre Oliveros</span></i>")
ggsave("Doing_Business.png", path = getwd(), width = 15, height = 10, units = "in", dpi = 300, limitsize = FALSE)






