# Install necessary packages if not already installed
# list_of_packages <- c("shiny", "ggplot2", "ggiraph", "leaflet", "dplyr", "tidyr", "readr", "sf", "RColorBrewer","shinythemes","plotly","ggbump","fontawesome")
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# Load Library 
library(shiny)
library(ggplot2)
library(ggiraph)
library(leaflet)
library(dplyr)
library(tidyr)
library(leaflet)
library(readr)
library(sf)
library(plotly)
library(RColorBrewer) 
library(fontawesome)
library(shinythemes)
library(ggbump)
# read data / data cleaning
City_Data <- read_csv('City.csv')
Weather_Data <- read_csv('Weather_Data.csv')
# remove NA
Weather_Data <- Weather_Data[complete.cases(Weather_Data$MinTemp,
                                            Weather_Data$MaxTemp,
                                            Weather_Data$Location,
                                            Weather_Data$Rainfall,
                                            Weather_Data$WindSpeed9am,
                                            Weather_Data$WindSpeed3pm,
                                            Weather_Data$WindGustDir,
                                            Weather_Data$RainToday,
                                            Weather_Data$Humidity9am,
                                            Weather_Data$Humidity3pm,
                                            Weather_Data$Pressure9am,
                                            Weather_Data$Pressure3pm), ]
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


# add Winddir
Weather_Data <- Weather_Data %>% 
  mutate(WindDir = case_when(
    WindGustDir %in% c("E", "ENE","ESE") ~ "East",
    WindGustDir %in% c("N", "NE", "NNE", "NNW", "NW") ~ "North",
    WindGustDir %in% c("S", "SE", "SSE", "SSW", "SW") ~ "South",
    WindGustDir %in% c("W","WNW","WSW") ~ "West"
  ))
Weather_Data$RainToday_encoded <- as.integer(factor(Weather_Data$RainToday, levels=c('No', 'Yes'))) - 1

Aggregate_Rain_Data <- Weather_Data %>%
  group_by(State,City,WindDir) %>%
  summarise(SumRain = sum(RainToday_encoded == 1), 
            SumNoRain = sum(RainToday_encoded == 0)) %>%
  mutate(RainingRate = round(SumRain / (SumRain + SumNoRain), 2))
# Calculate mean value
Aggregate_Weather_Data <- Weather_Data %>%
  group_by(State,City,WindDir) %>%
  summarise(Avg_MinTemp= mean(MinTemp),
            Avg_MaxTemp = mean(MaxTemp),
            Avg_Rainfall=mean(Rainfall),
            Avg_WindSpeed9am=mean(WindSpeed9am),
            Avg_WindSpeed3pm=mean(WindSpeed3pm),
            Avg_Humidity9am=mean(Humidity9am),
            Avg_Humidity3pm=mean(Humidity3pm)
  )

Aggregate_Weather_Data <- merge(Aggregate_Weather_Data, City_Data, by = "City")
Aggregate_Weather_Data <- left_join(Aggregate_Weather_Data, Aggregate_Rain_Data, by = c("State", "City","WindDir"))
Aggregate_Weather_Data <- Aggregate_Weather_Data %>% gather(key = "Time", value = "WindSpeed", Avg_WindSpeed9am, Avg_WindSpeed3pm)

Aggregate_Weather_Data_long <- Aggregate_Weather_Data %>% 
  gather(key = "Time", value = "Humidity", Avg_Humidity9am, Avg_Humidity3pm)

Aggregate_Weather_Data_long <- Aggregate_Weather_Data_long %>%
  mutate(Time = case_when(
    Time == "Avg_Humidity9am" ~ "9",
    Time == "Avg_Humidity3pm" ~ "3",
    TRUE ~ Time  
  ))

Aggregate_Weather_Data_long <- Aggregate_Weather_Data_long %>% 
  group_by(Time) %>% 
  mutate(rank = rank(Humidity, ties.method = "random")) %>% 
  ungroup()
# popup info
makeweatherPopup <- function(row) {
  paste0(strong(row$City), br(),
         'State: ', row$State,br(),
         'Raining Rate: ', row$RainingRate)
}
Aggregate_Weather_Data$Popup <- by(Aggregate_Weather_Data, seq_len(nrow(Aggregate_Weather_Data)), makeweatherPopup)

##################
# USER INTERFACE #
##################

# Weather tab
Weather_tab <- tabPanel(
  title='Weather',  # Title of the tab
  h2('Weather in Main City of Australia'),  # Header
  selectInput(
    'State',  # ID for input
    label = 'State or territory',  # Label
    choices = c('All', sort(unique(Aggregate_Weather_Data$State))),
    selected = 'All'  # Default value
  ),
  fluidPage(
    tags$head(
      tags$style(HTML("body { background-color: #f4f4f4; }")),  # Page background color
      tags$script(src = "https://kit.fontawesome.com/a076d05399.js")  # FontAwesome for icons
    ),
    tags$i(class="fas fa-cloud", `aria-hidden`="true"),  # Cloud icon
    leafletOutput("map_australia"),  # Map output
    helpText("Click on marker for more information"),  # Help text
    fluidRow(
      column(6, plotOutput("barPlot1")),  # Bar plot 1
      column(6, plotOutput("barPlot2"))  # Bar plot 2
    )
  )
)

# Temperature tab
MaxMin_Temp_tab <- tabPanel(
  title='Temperature',  # Title of the tab
  h2('Temperature in Australia'),  # Header
  sidebarLayout(
    sidebarPanel(
      radioButtons("state2", "State:", choices = c('All', sort(unique(Aggregate_Weather_Data$State))), selected = 'All')
    ),
    mainPanel (
      uiOutput("dynamic_image"),  # Dynamic image
      plotOutput('plot_temperature')  # Plot for temperature
    )
  )
)
Humidity_tab <- tabPanel(
  title = "Humidity",
  h2('Humidity in Different State'),
  plotOutput('plot_humidity')
)


About_tab <- tabPanel(
  title="About",# Title of the tab
  div(style = "text-align:center;", 
      img(src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAARwAAACxCAMAAAAh3/JWAAAAwFBMVEX///8JQYPd5OyClbYAP4IAMnwAPYEANH0AN34AOX8AL3vx9PgAO4D5+/319/oAM3zp7fOquc9ZdaLY4Orq7vQALXoAKXiis8vCzd3M1eJ6kbRjfqi3xNacrcdIaJqVpMHQ2eUzX5XFz95hfagUSokAH3WInbxNbZ1wia8xXZWer8h7kLIhTYoAJXcmU44AR4kAG3Nhha86ZpuMpMJcdJ9EY5YAHXWzvtEAFHGnscd5lrpIcqRUfKoADXCHmLcsUIv1YewpAAAVq0lEQVR4nO1dWUPburaWFc+2jKfE8xjXQzBOSZrr03Yf+v//1dWSHQiU7pe72UCuvwcyoBDrY81akhFasGDBggULFixYsGDBggULFixYsGDBggULFiy4AmjvfQEfGE73Gjuaa5qa8covjO3/IzJJp9vk6aUWxLuw+cUp6lqV9WpoVlaJL8dvv0b/9iW+H6JUS5UswKZTWu2B32z65jaJAyo2Gn1rzNpCuVN7O/MdjRBsVZ3D7d77mv8teJKL0NioX758UYvb77772qB8zGyOjvjyPxUlJrjLH3/jjtm/daX/PvKv5fSEaE+65Qaxl9UUyVjix7eJifFkhBJlenTqodtfKTl02oZC51avZgaI5lh1dBA3qiDJAElQN1SlVrvgkTptNyJkpwiZ2eFkmagyyJ+/4RMjCnN7i5Clyupg367aphfWN7Iics8h6rKkqlXa1lkdprKg54jsW7vLTPo36sJ23nseb4JwFfVNbYmUDV5XZEXnuT8DRlDoHKd3QVisGCUkGbor9V1Biwxshd8qVVD+jpdLikTlRhjaGEIdI/cy3+jM957G28BUEvaoYasd1pKsi/wTR6LCXbziqWrdqKo4NLces8paYFkOpaj23ncOb4ZTldTn+I7kXt02XaVvVAk44btWkaSbiR5FldJVMgbmOWIOduNkw632fS79zREW2EbxMZ9eEdNh0yVBMijUsOyOLgRBwM4mYu7ejBkCSs1sg/P75l2u/O0xCi5aBXSGSTLNlSYOsWc5kFDwnP4zSbIc1Xuek4AazUuCyWfnhxWlTcut+oePr5QcRyhBsdiEg93O881p7mWNUCbzg11iJz6sSl31gRprFivLi7Ebt8exhFA6vk6tIgMloZXkc86p3VMLG4Ot1WLkq3pkHMbYC4+t1BDkJ2BsiJMkORuMj9PfKPfX6aoiGxSL5+QimN5wcrDKcWwgE2FFbxwzaRvTPgmJcw9OzRw9fxqJfySmScVql1Ti+F7X/5YwBhoal9TwcnKajA4IxuyTqXsOTPsG9CXPqXdXS5L7VmDNGSl14S12Ar/MsdYe7eoqcwftRKdfy9Rn84npW9Yu+ZGMXklZMqwcZcLaQTFCOsdXlK3c9yDqIzhOrJwEkwThQxIO+O+/5bNCq0aksTBPaqfSnkeIY1kl+C7tdGOn1J4MijzFibHlxKNVgtEGE1Ti3KM2ib9Ok0MRU5VoQbE4eR/6cexbkBWQiQynE+7Yg3yIQFBwPE4BoPdXFBjkR+ngzqiu0uIwkCpH8c0UAd+lDol9RKjaWEBCxNk2GOq4i1HZSLWDktik2pdFiZ9YxUFDuVjG/VUanAntAzKBHHEfHT1VDyMxdDUQBjMNkQul0NWKyZFh2fumH7a1FUD4F8bEku5KZNfve/1vCusbWFxO7H7s7DZWCwPFgz0iEikW/eXP3KsilKG5DMaCaCdr2+YAAfPpgMg+eM+rf2MEHUK9yOn1Q/tjbOT7sKH0dKshJIhYrTt6VHgSaz8cbr1cM8y47aLYBGud1L3QIkO5UlfF4FCbUYhc/aOxmkFR2joaqO0FSuKOTtwEH5YEtZnHq6YoTkUAiTj15UHW63KGXPlqfRUF3hvoIPbNLmtptPPLOiWndZNT0hrbOs5jsp9n8cBcO/4Mk126UWWajO4Q1l9dqLgSOHtCyeH0bF/xnLINbU/mdHGXSQnCNEIkVumh1XYejPNBlnRFkqYKs/QdOfw1r3zmAyMHgmRO745NMMALSeYDgpI0DrzWCw6zdBBT3F+WSyk5WLlmySlPzCBzNErmD1bhb+dlB75yURBZDsa+ZWeTtzpE/bMqM9gc9ZoN8o6qjDxPVnf+yoRzvbhq4jam1tZCpSGAD7fcW4F7BoV6K+E612QmtCuEb2BBoa+UtFDkx5nzuu5Y2ER++6Met2irWUP3YnVC7BDpy/eewRsiHZF/w/FDkdTD8HwlT1nVxzxnZT471OXTbytavITQ9koXggHaHqPvErXGGBGtmmevzw+N7yRTgSdfU9nifoOAUX2ly3kALGoolDneptbFTdn8damVmXYJU77NfHXzCjV0hI+s07te/5simDy52FLTMbbAgBiGbluD8OjMEWHw1e4ovUYOjQLz/fUGOh7VCnDjQhj7W1a6kG36dgvVwY4Vb5j45O7wmujoEXKvOLlqa5oBQIwziPwweSDwzc5Aw+XJmrCShDvGwivkiAUig/+e1/+maGgYwzSGn9fE+X1nIpz2VISm1Oqe/Uy0Rv+dHL5C6Fvyntf/liBDjrwX5oS3OkvkIf4lNDB2J3LID1N9pQdDNFEYvvMc3gzgyVfyixnLOisqi4M9ZMi6R8z0mMey+r11Rw5QdrW+3Ow1FL2iL4AbC8UJuq/rgw8rvk5c1pX8gh4hRvGVrpRTL9QRlP6BHDlE/qrJDh26d+6pWcalk0fSc6clWTStf62J+xoQpwi9zJjO0O2cstdbW5LbOBohGjSQfxIuh9O8HHPXGuhYNKhhEYz4O0NK2FKDzNouRtM6924Rq7sQHpmmreK1VnR2Ns2reW6/3rabxxnzU9VGtP1HL50YXhKXU3XiWD0RqbTIVK61ipxskUGpqALH8c/pky6PGyZMJ9Q+1o53SMN+EppBXubWk+/Xr5wcd8/z0JqEJX42NRg5JwiaO6Kx4rFBJcaP4Zmb3adWfaFX+j1yxWslZ7dFJsezWJhY3MSOaqEaZANyq8SCUbDEuZsX71aqfGFzqFpdLzneNwRVHNU2tGgzmxKxVab0HHZgHZifBorOPZXtZcxI3b2pXys5I43gfomc2HTdU8VG4VmEzA9UYDBTLBeUamaHXObnck1tzrV6q7JA6CSCJ3+l0Md6/WqWdPvgqLycfQaLT0NpEOhcbUEH0oepPecVsO5aVLBlmSNrWJ8+VKpPQ2hmcbWlQEg8rVeLfOCKTsBIzhJL97LTZFzPI3j9qovIpxjhP+0H4addES3z4vEcIudAWD3Ljjhc9fIDVGOKl+bm7LaaIwSBxrRvuM2nT1jwnj2potwi44obdMreQN7zCqion6ZMS2wIKwb6U1HiMLvsBHLQnhEqlCgXr9UeU1siO4g8NzppH8jFtGeGGCwIrJk/14a5+w/a+XOIp3mOBoX2+138myOiehVexnUSmN6avUNj34mPli175ufCjdNoaKSKJSWI8PE7XfhbwNW0Z5tVS91A2oXo8Kx19sgUSz2vgxsvjiu4NUnS8ZxgoPiatEpTi24o7DZzzmLQQdPfk9Xh08RA5m4AcuRz7XxaFH7k1DmgjCYbArVIxer8h7FVh230uX3XAHujcZnZh3ZyM6NOnjksXQwyfz29ntOmaQtnNld38EPnEEoe3xNUbtgIJ7FTe2X5OT5+6oWax62qWrk9MHvRUSPjXLSeVIPQsQBPkdbD1t62YXFHVS2vpk9625IgS+BYnaMHwYmbJjnvyn/41Caou0gTsQ16E3yhkYv3lBLwVCh0ab3frhLL98vYylZR30WSN25phhVAQBzTMQLlJZENZNjRRf9S+6nXP59v/2lBC0Kqamj1ZHZESbAT/GycgXfbzRdO08a+b6NO5jm5Icj5D5UT+5ki1Z9ZcrTi+csUfnIgQNuZHVGtElAgM848dByqFq2m3cOaNUi6JFJ1o8Zaof6L7MPf9jB+6hM/vOcXb7JOCvN/wBsxdnihg/+932SO7nclVAJXbVyj2gZ9CU7qZKmBG9QAs95zC+zu/6WJvAWGZ2GJc+imva9fgJFbmZN6phZ54ThGterchBuQ09vubegcDET58QsaGvM3DeU0YhGzxefPviB9/vIzYfX0fyb5Mb3PmynciTcQ+m3VB4JAhUA5SOcVWpYYpo92hwNwWt+B5U14mae2mLR7Ztq9VRONF4zj7t+bzT+LTLWpa/Zg/knTlhrSmnO+JHlaW+RwGgoNi51K8+vG8eqdbbdhk1qN76BdlGpu5yDTprRoTT95dqp4+apoWIuThmMvGT7pRmrXx66Jy6RJ4/DIWHHP5CBH5zKkBcj5mVLN8roGm2d5IAZy7PS+uB9IeIc1E419PdhzSDCtT+C2MfzosK290Vt98oMtcMRNTx4lx2xtjOK08IwhGiyWKmhOvKtXq3rnw1keyDfNfXCwy3RPk7FWPMc2EznUYA3b8ko6CmJpKnu6KSPHyHoLmaaQ1zUqTEwnjrOml6eTlyRB7qnzooQd07j/OYxs+2tXT1RkEzna8GltzW9oy2MRw4FUUMfKH6rMQFaHY+F2SGpqS+JCvVOezobheRoXDh6hwnSMoX5T5MioKxb+MHIMS9wVV7NC05gIh31vR9y22NtUFtwfJy1HjWdQgxEPLTaS/UWbG69stkG298CHUUaOrKZu7Irqdty21m2nRBht83ee0z8FLWU6QXLf98Hp4KjD3X40HBrWOcV/YDMa9eeDMOfqshDCKIOrWCKPZdBFOKyL+F6WZRYbXlvvMpV/Ho6NnDCZfDGJ42PRazQojjkfkbDynVDmd8CeVUGyrsstaEwQ3RXJoaXvH8BehXdB+ZhkOscrOgvFypC/jSXNHkJySkIDZQ804t1qKOiZoSbjsK4pI8aDrN+k4Jj8QmjhcTdQAiNbu20bLfTQIW7bEVdbUMz0fef0jyEs0e5UD3WG7DFtb32aErnIQEbbPbajl41whOM+oFBIfdM+maMe3LQaKrMorEiF0T4/OIVfjDTwNtIrKZg21H4mgRaVqN5puf+FWowADPGz40aDtIpZwINtMbkIYSyammpEO+AK+VFuo+RY21B0vZKjAklKyQmgqJcPThmxkhcKGvvlLoZRga5Ab90+99Ja1EDVi6TtUAaNYY/+qaVZ+ueucz0h7qb/skcnpDngduKmeWXLndsMuBV+r19RImF0kFPxsmsaMWKDKuCVqBUKTo3lnNMqPG6r82LvS4R3w6u9SUFU3V6kC8S3uyvaPeNkDScORTHo/KH+m32a2Z8SJjK2PWc/JN+/P9jcEF7dirmBHQf/X7JFI/dHa4zzK8k4FyxYsGDBggULrgZVY9t2U/g/GqjUJPa9Ybb0je10BLYd0eTA2W4xSliIW24j+rT5YfyAygTSWjvf0c8fcuTZ2/vb6YiCv34OOYrte60c7G203R4sG9YGyf1fUwZhhn/99RPqgh79osNHDhBzwxGwMQbGV3blUIXw9waZl8xP7DGD2z7I7Pewlu6u6dOBpeoRHc6PKKZ56hCjmJ1e1psooG+fdijBhiNhw8POVwcKOlPyHkCpnUQH+nQYyQePnk24VQrS7tjkfwE5T4dCpoycHf15LNhWByuDD2AoV4DkwHrLfsrKocGg9ykHssGqPCe2djcfS5V0j90nUAMDDA/TZz42TAlmTe4cYhjw/6fkPPb3pWztHMSlDqJem8hhx1OQdTy9QnPJCybKKh2ngglDekkOqkZzPvUsHqZHS6bjP3yJ50xOu6LQgRwljOatDCdW+5vIQU1HWAvBdHbHKmUrOAgVdgiPncWOf6N8VOyM3+fkBFU7FwKzuZZc3mnoZIfpR1crtq4yqRWTnMo153WUlJEDalX7yOhSFD+Sk6smZutTnWVYlIHDt2yfsw9pHRyP/Jwc1JyPy8vm9qhyQ+Cj48cugZ0l54mcJ5uzZRIE97QAi6FV2ydy0Ck5Mg82TGcMdSM6zj3spPr+Gzm357ZcX55kZSw+j825lBxqc+ZG7YxtawBTUgMFGt8n0PLPhlrcJAxgkCe1Qg0VpYDOfbV7JEd6SQ6qptNThvIzkIP/y1r8/stsgujBkfSBWU/FT21/RIRtbQiZgrkCeKsvTGK0+Viqde0GVMD2dIAmtiiukQu9OQNzdMHdXEWNHrddYfnBQBr0UZL9R+9GIdYuo3qf7BL6P453mYOyXfYwH4uIjGwLJzEgJ8tyeG3Sn+OO3ZcJTcdm04/UDzny4e4YyN1lwcPPW/rrMsmAwiRha6P088mjlJBd9NcDCN/0mQULFixYsGDB/wFk9b742Hsh8Dvjvee/YMGCBQs+BYjz8qk2laOIO92De74PN3tqGM8+mednbwNj2DiisWqQ8XL0o1uif52OufiOjwzz6Wik6RxRN0l//YIdDRFsABl+9dMtPPGu+DUUVXq+mZd72zd2tx6gIRmZu9Ovnu1Ed5Khb0vk179+9RXf1efZp3PllXhRn1qu19Dv4Hn+XD39oHC+Pm7PS6ddi8hTdWxijxXvmpvHozpqtUPucDdVqGI1AlLNYsOxNYTv6vlAQFtlT4q1hdxmc94YeXpsrHS+JOzzYuCa1u0bzuwfwKhWs/QHwlzftQQdpsT+17b0WMPLhIJOas2WD/IvZ85SWQHJGoXzbWUigUnLSaUyZuzVmfpOl+aFGHMDf9hXebYj8u0m9k+gloW5+drWN5NlmMmZ3nxBjrlmurcX8vldV2LHsv9GTroGDlp1Lh53oi5MjZfu+oKcD45IFqed9rnKzaL/d+TkbB91vK4eR7QyVNz/QE64mSVnCE+Kwthx1U9DDjlFvcrMyDayZx2wBNE0DJPZ4WfkdMTdMklopafd+dYNHFvxB3JsbrZEfWb2igwm22WVeV/lHDJ/x4eFpvu3EizROne4VacTXawbvhv6NbMHl+RI3LbYsKMpOuXphLtAvbFeJWeEOzPPZpjsa+TsdSmHpR22b1/lh2FQP/YuLPzFcRS4xtZGtTAdv0UlxzHNFVOCF2pFElX3gZwnyclVEIXfyVGKtt2fPSGpajqSU5QcaTM5XGni7GOvXPnU8dpyhcy7HFlqxd6jNoe6LZPZhJc2B4UCNVFb+cnmBKqaPydn6iS4Cct2XcyekOxBKANF53LC++hsc/DHVqsdnWUgq3FIhaYUpnlZAv9Hg4ywtAnoCOmxYX9Uof8iFs73Btnq7CEFcUzVOcoj7LQQlAt6FVQgkp/CIK/AeNjKAJPF+ob9J/+WHG3Y+MgQlMfYsN3AijpWz3HMMPVRnIAcrAhTrY/o0zZGh7KjAK+fgpxvMMlcUGBGbq8yG0BtDkhQkNMfNotiEPbP5LiVSm2KtRHmtVx3Pd10ppKm6eOvk5E9MR84rlVm18/koFwW1QtyPnZn1366e9eGWd+TwNoqkhsBrh9OUqJ2FU7mIpA3rSQwSclXFjPW6oatGZvVMLfgrFUQElzNN3roJpmp1TUjXPg2f6HDb3IE/wAZWIs+cgvKUTrExvlwEnLi9ruc5p/cXhD5NRy84PUcp8riRjZJ3HF8ZRfn1oiyESS7tfePJfKy2/BNJ00RsZlwXAd9O+hYrdvYSZTem9Uoh/vs+Q3HSSKvSh/5vsuOidmus0m6DRc7JnKwaWLYTgRVCfP8nNC36ZOLvWc4KMv8cnJ5WQZzFq7RD2J2yAXbmeTSl845QYdtStN3YOfj250FCxYsWLBgwYIFCxYsWLBgwYIFCxYsWLBgwYIFr+N/AfD+4k4OlWkCAAAAAElFTkSuQmCC", 
      height="150px",width="200px"),
      p("This website is developed for investigating Weather information in Australia. You can find more information on the website:", 
        a(href="https://github.com/rfordatascience/tidytuesday/blob/20cd5a683be5f3315299c907e985ec6b6c2dde87/data/2020/2020-01-07/readme.md", "here"))
  )
)

ui <- navbarPage(
  id='mypage',  # Unique ID for the navbarPage
  title='Weather in Australia',  # Title displayed in the navbar
  theme = shinytheme("flatly"),  # Using the "flatly" theme from shinythemes
  
  Weather_tab,  # Tab for weather-related content
  MaxMin_Temp_tab,  # Tab for displaying max and min temperatures
  Humidity_tab,  
  
  tags$head(  # Adding custom HTML in the head section
    tags$style(HTML(
      "body {
        background-color: white;  # Setting the background color to white
      }"
    ))
  ),
  
  About_tab  # Tab for about section
)


################
# SHINY SERVER #
################



server <- function(input, output, session) {
  # Reactive function to filter weather data based on user input for 'State'
  
  getFilteredWeatherData <- reactive({
    filter(Aggregate_Weather_Data, 
           if (input$State == 'All') TRUE else State == input$State)
  })

  # Initialize the map
  output$map_australia<- renderLeaflet({
    # Create map using filtered data
    
    leaflet(getFilteredWeatherData()) %>%
      # Add OpenStreetMap and Stamen.Toner as tile layers
      
      addProviderTiles(providers$OpenStreetMap, group = "OSM")%>%
      addProviderTiles(providers$Stamen.Toner, group = "Stamen Toner") %>%
      # Add awesome markers for each city
      
      addAwesomeMarkers(lng=~long, lat=~lat, label=~City,popup=~Popup,
                        icon = ~makeAwesomeIcon(
                          icon = 'cloud', 
                          markerColor = "blue"
                        )
      )%>%
      # Add layer control to switch between "OSM" and "Stamen Toner"
      
    addLayersControl(
      baseGroups = c("OSM", "Stamen Toner"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  # Define a reactive expression named 'filtered_data'
  filtered_data <- reactive({
    # Check if the selected state is 'All'
    if (input$state2 == 'All') {
      return(Aggregate_Weather_Data)
    } else {
      # Filter the Aggregate_Weather_Data based on the selected state
      return(Aggregate_Weather_Data %>% filter(State %in% input$state2))
    }
  })
  timer <- reactiveTimer(10000)
  output$plot_humidity<-renderPlot({
    timer()
    if (as.integer(Sys.time()) %% 20 < 10) {
      ggplot(Weather_Data, aes(x = City,y=Humidity3pm)) +
      geom_point(aes(size = Rainfall, color=City), alpha = 0.6,na.rm = TRUE) +
      ylab("Humidity ") +
      xlab("City") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank()) +
      # Use the Set1 palette for the color scale
      scale_color_brewer(palette = "Set1")
    }else{
      ggplot(Weather_Data, aes(x = City,y=Humidity9am)) +
        geom_point(aes(size = Rainfall, color=City), alpha = 0.6,na.rm = TRUE) +
        ylab("Humidity ") +
        xlab("City") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank()) +
        # Use the Set1 palette for the color scale
        scale_color_brewer(palette = "Set2")
    }
    # Convert ggplot object to plotly object and specify which data appears in the hover text
    
    
  })
 
  # Define a renderPlot output called 'plot_temperature'
  output$plot_temperature <- renderPlot({
    # Create a ggplot using filtered_data() as the data source and 'State' as the x-axis
    ggplot2::ggplot(filtered_data(), aes(x = State))+
      # Add points for Avg_MinTemp, colored by 'Min Temperature' and shaped by WindDir
      geom_point(aes(y = Avg_MinTemp, color = "Min Temperature", shape = WindDir), na.rm = TRUE) +
      geom_point(aes(y = Avg_MaxTemp, color = "Max Temperature", shape = WindDir), na.rm = TRUE) +
      ylab("Temperature") +
      xlab("State") +
      ggtitle("Min and Max Temperature by State") +
      # Use a minimal theme for the plot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank()) +
      # Use the Set1 palette for the color scale
      scale_color_brewer(palette = "Set1")
  })
  state_to_url <- list(
    "Australian Capital Territory"="https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcSwaPqPKX2t28MduVIyETXXKu4fMh2AXQNeTfWBc6LuGX5vcUVzWmk17OvSFEQKVm6tTAxojJIjpgKJjh-vs8qTGpZr7oV2",
    "New South Wales" = "https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcTDb23kCZSFnO7jwrj_IepTvkQWURH6X8LPYIvFBBO9YYKTyHuQrKHgd5xYXA3PSPYQCg9khGCiYEJrz__TARy4Y6SK2bOQ",
    "Northern Territory"="https://lh5.googleusercontent.com/p/AF1QipMcF-VjyMIowfYIWvkM-wqzMBtt7jzhnMP4rRrc=w548-h318-n-k-no",
    "Queensland" ="https://encrypted-tbn1.gstatic.com/licensed-image?q=tbn:ANd9GcQeEiKxva8oR4AVNRfc4EUXg76h0VpVER9xeVsquJ5LctZAW8Yre3i1dnd41PRaQIRwZf0houAXRQUOZY9VnpLY7AuP2AvF",
    "South Australia" = "https://lh3.googleusercontent.com/proxy/qqWjB5kaV7LLHhrrcacZRxnpicOUfWRPbv9ozTslfHPZLi8sUm_O4CfPGU8iJsQ2swWjdI4gjHjVqyf9bCTdoCtLk7coXLOMiBqifAKHE3YYlLCJGdEOPeMlBM--THsUq3uiNSdp1fcUtn9YDB6kXT9BZ60xGO8=w548-h318-n-k-no",
    "Tasmania" ="https://lh5.googleusercontent.com/p/AF1QipONslsVm9Oue2rOFjqzWzLH_IcJxnN9XqRL8kLw=w548-h318-n-k-no",
    "Victoria"= "https://encrypted-tbn1.gstatic.com/licensed-image?q=tbn:ANd9GcQVNyIGx6NSvkwTPsPGUBKJSLo6nOH6Iue964omNT8SQsvdy9m4LxHOOQEhkS5AzvoLy2hWh1P78Dwc1x9Q8s0tz9Gr5Hkf",
    "Western Australia" = "https://encrypted-tbn2.gstatic.com/licensed-image?q=tbn:ANd9GcSZRQpPRMrmb2CNqJxoDzv28wuT8FwmR22fIQag3gEJbxsAZDQUv9hvw9aem6FpTwEuUGGUI5CofA1mD7oGAcrotlYTZYHp"
  )
  output$dynamic_image <- renderUI({
    if (input$state2 == "All") {
      img_urls <- c("https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcSwaPqPKX2t28MduVIyETXXKu4fMh2AXQNeTfWBc6LuGX5vcUVzWmk17OvSFEQKVm6tTAxojJIjpgKJjh-vs8qTGpZr7oV2",
                    "https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcTDb23kCZSFnO7jwrj_IepTvkQWURH6X8LPYIvFBBO9YYKTyHuQrKHgd5xYXA3PSPYQCg9khGCiYEJrz__TARy4Y6SK2bOQ",
                    "https://lh5.googleusercontent.com/p/AF1QipMcF-VjyMIowfYIWvkM-wqzMBtt7jzhnMP4rRrc=w548-h318-n-k-no",
                    "https://encrypted-tbn1.gstatic.com/licensed-image?q=tbn:ANd9GcQeEiKxva8oR4AVNRfc4EUXg76h0VpVER9xeVsquJ5LctZAW8Yre3i1dnd41PRaQIRwZf0houAXRQUOZY9VnpLY7AuP2AvF",
                    "https://lh3.googleusercontent.com/proxy/qqWjB5kaV7LLHhrrcacZRxnpicOUfWRPbv9ozTslfHPZLi8sUm_O4CfPGU8iJsQ2swWjdI4gjHjVqyf9bCTdoCtLk7coXLOMiBqifAKHE3YYlLCJGdEOPeMlBM--THsUq3uiNSdp1fcUtn9YDB6kXT9BZ60xGO8=w548-h318-n-k-no",
                    "https://lh5.googleusercontent.com/p/AF1QipONslsVm9Oue2rOFjqzWzLH_IcJxnN9XqRL8kLw=w548-h318-n-k-no",
                    "https://encrypted-tbn1.gstatic.com/licensed-image?q=tbn:ANd9GcQVNyIGx6NSvkwTPsPGUBKJSLo6nOH6Iue964omNT8SQsvdy9m4LxHOOQEhkS5AzvoLy2hWh1P78Dwc1x9Q8s0tz9Gr5Hkf",
                    "https://encrypted-tbn2.gstatic.com/licensed-image?q=tbn:ANd9GcSZRQpPRMrmb2CNqJxoDzv28wuT8FwmR22fIQag3gEJbxsAZDQUv9hvw9aem6FpTwEuUGGUI5CofA1mD7oGAcrotlYTZYHp"
                    )
      # Create a list of image tags from the URLs
      img_list <- lapply(img_urls, function(url) {
        # Create an image tag for each URL, with a specified width and height
        tags$img(src = url, style="width:100px;height:100px;")
      })
      # If there are multiple URLs, combine them into a single tagList
      do.call(tagList, img_list)
    } else {
      # If there's only one URL associated with the selected state, show that image
      tags$img(src = state_to_url[[input$state2]], style="width:100px;height:100px;")
    }
  })
  
  # Observe if a marker on the map is clicked
  observeEvent(input$map_australia_marker_click, {
    # Store the clicked marker information
    marker <- input$map_australia_marker_click
    # Filter weather data based on the clicked marker's latitude and longitude
    clicked_city <- getFilteredWeatherData() %>% 
      filter(lat == marker$lat & long == marker$lng)
    # Render a bar plot based on the clicked city's data
    output$barPlot1 <- renderPlot({
      p <- ggplot2::ggplot(clicked_city, aes(x = WindDir, y = Avg_Rainfall, fill = WindDir, text = paste("Average Rainfall:", Avg_Rainfall))) +
        geom_bar(stat = "identity") +
        xlab("Wind Direction") +
        ylab("Average Rainfall") +
        ggtitle(paste("Wind Direction by", clicked_city$City)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank()) +
        scale_color_brewer(palette = "Set1")
      print(p)
    })
    output$barPlot2 <- renderPlot({
      p2 <- ggplot2::ggplot(clicked_city, aes(x = WindSpeed, y = Time,group=City)) +
        geom_line(color="#4DAF4A") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank()) +
        scale_color_brewer(palette = "Set1")
      print(p2)
    })
  })
  
  
}

#############
# RUN SHINY #
#############

shinyApp(ui, server)
