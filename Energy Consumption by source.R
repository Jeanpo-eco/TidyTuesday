#Tidy Tuesday
#About: The objetive of this code is to compare the energy consumption en US by source, between 2019 and 2020, using the plot
#Dumb Bell from ggplot2
#Load the packages
library(tidyverse)
library(lubridate)
library(ggalt)
library(readxl)
library(ggtext)
#Tidy is for cleaning the data and ggalt for more plots

#Donwload the data for windows
setwd("~/economia/Tidy Tuesdau/Segunda Entrega")
destfile = paste(getwd( ), "Table_1.3_Primary_Energy_Consumption_by_Source.xlsx", sep = "/")
download.file("https://www.eia.gov/totalenergy/data/browser/xls.php?tbl=T01.03&freq=m", destfile, mode = "wb", quiet = TRUE)
remove(destfile)
#Open the data
Consumption <- read_excel("Table_1.3_Primary_Energy_Consumption_by_Source.xlsx", sheet = "Monthly Data", skip = 10)
#Keep the necessary things
Consumption_source = Consumption %>% filter( Month >= as.Date("2010-01-01") & Month <= as.Date("2010-12-01") 
                                             | Month >= as.Date("2019-01-01")
                                             | Month >= as.Date("2000-01-01") & Month <= as.Date("2000-12-01"))

Consumption_source$Month = ymd(Consumption_source$Month)

Consumption_source = Consumption_source %>% mutate(Consumption_source, 
                                                   Year = ifelse(Month < as.Date("2010-01-01"), "2000", 
                                            ifelse(Month >= as.Date("2010-01-01") & Month <= as.Date("2010-12-01"), "2010",
                                            ifelse(Month >= as.Date("2019-01-01") & Month <= as.Date("2019-12-01"), "2019",
                                                   "2020"))))

Consumption_source = Consumption_source[ ,c(14, 2:13)]
Consumption_source[ ] = lapply(Consumption_source, function(x) as.numeric(as.character(x)))

#Eliminate Dec of each year to be consistent in the data and sum

Consumption_source = Consumption_source[-c(12, 24, 36), ]
Consumption_source = Consumption_source %>% group_by(Year) %>% summarise_if(is.numeric, sum)

#Round to two decimals

Consumption_source = Consumption_source %>% mutate_if(is.numeric, ~round(., 2))

#Transform the Year col to Date
Consumption_source$Year = paste(Consumption_source$Year, "12-01")
Consumption_source$Year = ymd(Consumption_source$Year)

#Separate by source and totals
Consumption_by_source = Consumption_source[ , -c(5,12,13)]
Total_Consumption = Consumption_source

#Transform total consumption to percentages
percentage = function(x) 100/ Total_Consumption$`Total Primary Energy Consumption` * x
Total_Consumption[c(2:11)] = lapply(Total_Consumption[c(2:11)], percentage)
remove(percentage)
Total_Consumption = Total_Consumption %>% mutate_if(is.numeric, ~round(., 2))

#Make the Plot
Consumption_by_source = gather(Consumption_by_source, colnames(Consumption_by_source[c(2:10)]),
                                                         key= "Source", value = "Consumption")
Consumption_by_source = spread(Consumption_by_source, Year, Consumption)
Consumption_by_source[ , 1] = c("Biomass Energy", "Coal", "Geothermal Energy",
                                    "Hydroelectric Power", "Natural Gas Consumption",
                                    "Nuclear Electric Power", "Petroleum Consumption",
                                    "Solar Energy", "Wind Energy")


#Make the Plot
ggplot(Consumption_by_source, aes(y = Source, x = `2000-12-01`, xend = `2010-12-01`))+
  geom_dumbbell(size= 3, color="darkorange4", colour_x = "#B8860B", colour_xend = "#DAA520", 
                dot_guide=TRUE, dot_guide_size=1, dot_guide_colour = "yellow")+
  geom_dumbbell(aes(y = Source, x = `2010-12-01`, xend = `2019-12-01`),
                size = 3, color="darkorange2", colour_x = "#DAA520",colour_xend = "#FFD700")+
  geom_dumbbell(aes(y = Source, x = `2019-12-01`, xend = `2020-12-01`),
                size = 3, color="darkorange", colour_x = "#FFD700",colour_xend = "#D2691E")+
  theme(panel.grid.major.x=element_line(size=0.05))+
  facet_grid(rows = vars(Source), scales = "free")+
  theme(strip.text = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect("beige"),
        panel.background = element_rect("tan"),
        plot.subtitle = element_markdown(size = 10, hjust = 1.5),
        plot.title = element_markdown(size = 15, hjust = -0.75))+
  labs(x = "Consumption in Quadrillion Btu",
       y = NULL,
       title = "<strong><span style='color:#ff8c00'; font-size: 18pt> Primary Energy Consumption in U.S.</span></strong></b>",
       subtitle = "Comparision of consumption by source in U.S. between the years 
       <strong><span style='color:#B8860B'; font-size: 15pt>2000</span></strong>, 
       <strong><span style='color:#DAA520'; font-size: 15pt>2010</span></strong>,
       <strong><span style='color:#FFD700'; font-size: 15pt>2019</span></strong> and 
       <strong><span style='color:#D2691E'; font-size: 15pt>2020</span></strong>",
       caption = "Source: EIA | Created by: Jean Pierre Oliveros")
ggsave("Consumption_by_source.png", path = getwd())





