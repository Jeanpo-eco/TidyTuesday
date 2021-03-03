#This code is is to build a plot comparing the monthly oil production from Saudi Arabia, Venezuela and Norway
#Source of the data:
#Saudi Arabia: https://www.eia.gov/international/data/country/SAU/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids-production?pd=5&p=0000000000000000000000000000000000vg&u=0&f=M&v=mapbubble&a=-&i=none&vo=value&&t=C&g=none&l=249--199&s=94694400000&e=1604188800000
#Norway: https://www.eia.gov/international/data/country/NOR/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids-production?pd=5&p=0000000000000000000000000000000000vg&u=0&f=M&v=mapbubble&a=-&i=none&vo=value&&t=C&g=none&l=249--165&s=94694400000&e=1604188800000
#Venezuela https://www.eia.gov/international/data/country/VEN/petroleum-and-other-liquids/monthly-petroleum-and-other-liquids-production?pd=5&p=0000000000000000000000000000000000vg&u=0&f=M&v=mapbubble&a=-&i=none&vo=value&vb=170&t=C&g=none&l=249--243&s=94694400000&e=1604188800000&ev=false

#Packages
library(lubridate)
library(tidyverse)

#Stablish directory
setwd("~/economia/Tidy Tuesdau/Primera Entrega")

#Load the Data for each country
data_ven <- read_csv("INT-Export-03-02-2021_19-27-16.csv", skip = 1)
data_nor <- read_csv("INT-Export-03-02-2021_19-29-26.csv", skip = 1)
data_sa <- read_csv("INT-Export-03-02-2021_19-31-01.csv", skip = 1)

#Cleaning the data

#Venezuela
oil_ven = data_ven[c(5), c(2:ncol(data_ven))]
colnames(oil_ven)[colnames(oil_ven) == "X2"] = "Country"
oil_ven[1,1] = "Venezuela"
oil_ven = gather(oil_ven, 2:ncol(oil_ven), key = "Date", value = "Data")
oil_ven[ , 2] = seq(from = as.Date("1973-01-01"), to = as.Date("2020-11-01"), by = 'month')
oil_ven$Date = ymd(oil_ven$Date)
oil_ven$Data = sprintf(oil_ven$Data, fmt = '%#.2f')

#Norway
oil_nor = data_nor[c(5), c(2:ncol(data_nor))]
colnames(oil_nor)[colnames(oil_nor) == "X2"] = "Country"
oil_nor[1,1] = "Norway"
oil_nor = gather(oil_nor, 2:ncol(oil_nor), key = "Date", value = "Data")
oil_nor[ , 2] = seq(from = as.Date("1973-01-01"), to = as.Date("2020-11-01"), by = 'month')
oil_nor$Date = ymd(oil_nor$Date)
oil_nor$Data = sprintf(oil_nor$Data, fmt = '%#.2f')

#Saudi Arabia
oil_sa = data_sa[c(5), c(2:ncol(data_sa))]
colnames(oil_sa)[colnames(oil_sa) == "X2"] = "Country"
oil_sa[1,1] = "Saudi Arabia"
oil_sa = gather(oil_sa, 2:ncol(oil_sa), key = "Date", value = "Data")
oil_sa[ , 2] = seq(from = as.Date("1973-01-01"), to = as.Date("2020-11-01"), by = 'month')
oil_sa$Date = ymd(oil_sa$Date)
oil_sa$Data = sprintf(oil_sa$Data, fmt = '%#.2f')

#Combine together
Data = bind_rows(oil_nor, oil_sa)
Data = bind_rows(Data, oil_ven)
Data$Data = as.numeric(Data$Data)
Data$Date = ymd(Data$Date)
#Graph
ggplot(Data, aes(Date, Data, color = Country))+
  geom_line(lwd = 1.3)+
  labs(title = "Monthly Oil Production",
       subtitle = "A comparison between three nations that have state oil companies",
       x = "Months", 
       y="Oil Production (mb/d)",
       caption = "Source: EIA")+
  scale_fill_discrete(name = "Country", labels = c("Saudi Arabia","Venezuela", "Norway"))+
  scale_x_date(date_labels = "%Y/%m", date_breaks = "3 years")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
#Saving
ggsave("Monthly_Oil_Production.png")
  