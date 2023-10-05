# Install necessary packages if not already installed
list_of_packages <- c("shiny", "ggplot2", "ggiraph", "leaflet", "dplyr", "tidyr", "readr", "sf", "RColorBrewer")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(ggplot2)
library(ggiraph)
library(leaflet)
library(dplyr)
library(tidyr)
library(leaflet)
library(readr)
library(sf)
library(RColorBrewer) 
City_Data <- read_csv("Desktop/DS/GEOM90007/Assignment2/Data/City.csv")
Weather_Data <- read_csv("Desktop/DS/GEOM90007/Assignment2/Data/Weather_Data.csv")
Weather_Data <- Weather_Data[complete.cases(Weather_Data$MinTemp, Weather_Data$MaxTemp,Weather_Data$Location,Weather_Data$Rainfall,Weather_Data$WindSpeed9am,Weather_Data$WindSpeed3pm,Weather_Data$WindGustDir), ]
# Since Location has 49 unique values
# Add State information
Weather_Data <- Weather_Data %>% 
  mutate(State = case_when(
    Location %in% c("Adelaide", "MountGambier") ~ "South Australia",
    Location %in% c("Albany", "Perth", "PearceRAAF", "SalmonGums", "Walpole") ~ "Western Australia",
    Location %in% c("Albury", "BadgerysCreek", "CoffsHarbour", "Moree", "Newcastle", "NorahHead", "Penrith", "Sydney", "SydneyAirport", "WaggaWagga", "Williamtown", "Wollongong") ~ "New South Wales",
    Location %in% c("AliceSprings", "Darwin", "Katherine", "Uluru") ~ "Northern Territory",
    Location %in% c("Ballarat", "Bendigo", "Dartmoor", "Melbourne", "MelbourneAirport", "Mildura", "Nhil", "Portland", "Sale", "Watsonia") ~ "Victoria",
    Location %in% c("Brisbane", "Cairns", "GoldCoast", "Townsville") ~ "Queensland",
    Location %in% c("Canberra", "Tuggeranong") ~ "Australian Capital Territory",
    Location %in% c("Hobart", "Launceston") ~ "Tasmania"
    
  ))
Weather_Data <- Weather_Data %>% 
  mutate(City = case_when(
    Location %in% c("Adelaide", "MountGambier") ~ "Adelaide",
    Location %in% c("Albany", "Perth", "PearceRAAF", "SalmonGums", "Walpole") ~ "Perth",
    Location %in% c("Albury", "BadgerysCreek", "CoffsHarbour", "Moree", "Newcastle", "NorahHead", "Penrith", "Sydney", "SydneyAirport", "WaggaWagga", "Williamtown", "Wollongong") ~ "Sydney",
    Location %in% c("AliceSprings", "Darwin", "Katherine", "Uluru") ~ "Darwin",
    Location %in% c("Ballarat", "Bendigo", "Dartmoor", "Melbourne", "MelbourneAirport", "Mildura", "Nhil", "Portland", "Sale", "Watsonia") ~ "Melbourne",
    Location %in% c("Brisbane", "Cairns", "GoldCoast", "Townsville") ~ "Brisbane",
    Location %in% c("Canberra", "Tuggeranong") ~ "Canberra",
    Location %in% c("Hobart", "Launceston") ~ "Hobart"
  ))



Weather_Data <- Weather_Data %>% 
  mutate(WindDir = case_when(
    WindGustDir %in% c("E", "ENE","ESE") ~ "East",
    WindGustDir %in% c("N", "NE", "NNE", "NNW", "NW") ~ "North",
    WindGustDir %in% c("S", "SE", "SSE", "SSW", "SW") ~ "South",
    WindGustDir %in% c("W","WNW","WSW") ~ "West"
  ))

Aggregate_Weather_Data <- Weather_Data %>%
  group_by(State,City,WindDir) %>%
  summarise(Avg_MinTemp= mean(MinTemp),
            Avg_MaxTemp = mean(MaxTemp),
            Avg_Rainfall=mean(Rainfall),
            Avg_WindSpeed9am=mean(WindSpeed9am),
            Avg_WindSpeed3pm=mean(WindSpeed3pm)
            )

Aggregate_Weather_Data <- merge(Aggregate_Weather_Data, City_Data, by = "City")


ggplot(Aggregate_Weather_Data, aes(x = State)) +
  geom_point(aes(y = Avg_MinTemp, color = "Min Temperature",shape = WindDir), na.rm = TRUE) +
  geom_point(aes(y = Avg_MaxTemp, color = "Max Temperature",shape = WindDir), na.rm = TRUE) +
  ylab("Temperature") +
  xlab("State") +
  ggtitle("Min and Max Temperature by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_brewer(palette = "Set1")

# Reorder State based on Avg_Rainfall
Aggregate_Weather_Data$State <- with(Aggregate_Weather_Data, reorder(State, -Avg_Rainfall))


ggplot(Aggregate_Weather_Data, aes(x = State, y = Avg_Rainfall)) +
  geom_bar(stat = "identity", na.rm = TRUE,fill="#377EB8") +
  ylab("Average Rainfall") +
  xlab("State") +
  ggtitle("Average Rainfall by Location") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) 

heatmap_data <- as.data.frame(with(Aggregate_Weather_Data, table(Avg_WindSpeed9am, Avg_WindSpeed3pm)))


ggplot(Aggregate_Weather_Data, aes(x = Avg_WindSpeed9am, y = Avg_WindSpeed3pm)) +
  geom_point(color="#4DAF4A") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) 

Winddir_State_Data <- Weather_Data %>%
  group_by(State, WindDir) %>%
  summarise(Count = n())

ggplot(Winddir_State_Data, aes(x = WindDir, y = Count, fill = WindDir)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ State) +  # Faceting by State
  xlab("Wind Direction") +
  ylab("Count") +
  ggtitle("Wind Direction by State") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_brewer(palette = "Set1")

##################
# USER INTERFACE #
##################


