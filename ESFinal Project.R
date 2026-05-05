library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

#Data ----
skenandoa <- read.csv("~/Desktop/ENVSTFinalData/SkenETdata.csv")
forest <- read.csv("~/Desktop/ENVSTFinalData/NeighbouringForestofSken.csv")
urban <- read.csv("~/Desktop/ENVSTFinalData/NewHartfordET.csv")
phoenix_nohouse_nodesert <- read.csv("~/Desktop/ENVSTFinalData/PhoenixGolfcoursenohousenodesert.csv")
phoenix_desert <- read.csv("~/Desktop/ENVSTFinalData/PhoenixGolfCoursewNohouses.csv")
phoenix_houses <- read.csv("~/Desktop/ENVSTFinalData/golfcoursewHousesPhoenix.csv")

head(skenandoa)

#Data Wrangling----
#Need to reformat date using Lubridate, rename columns for simplicity, and omit NA entries
# Skenandoa Golf Course
skenandoa <- skenandoa %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%       # rename columns
  mutate(date = ymd_hms(date)) %>%                     # fix date format
  na.omit()                                            # remove NA entries

# Neighbouring Forest
forest <- forest %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%
  mutate(date = ymd_hms(date)) %>%
  na.omit()

# Urban - New Hartford
urban <- urban %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%
  mutate(date = ymd_hms(date)) %>%
  na.omit()

# Phoenix - No House, No Desert
phoenix_nohouse_nodesert <- phoenix_nohouse_nodesert %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%
  mutate(date = ymd_hms(date)) %>%
  na.omit()

# Phoenix - Desert
phoenix_desert <- phoenix_desert %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%
  mutate(date = ymd_hms(date)) %>%
  na.omit()

# Phoenix - Houses
phoenix_houses <- phoenix_houses %>%
  rename(date = Month, ET.in = Ensemble.ET) %>%
  mutate(date = ymd_hms(date)) %>%
  na.omit()


#Data Analysis: ET for each region ----
# Skenandoa Golf Course
ggplot(skenandoa, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Skenandoa Golf Course (NY)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Neighbouring Forest
ggplot(forest, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Forest Neighbouring Skenandoa Golf Course (NY)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Urban - New Hartford
ggplot(urban, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Urban Area Neighbouring Skenandoa Golf Course (NY)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Phoenix - No House, No Desert
ggplot(phoenix_nohouse_nodesert, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Phoenix Golf Course with no houses, nor deserts (AZ)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Phoenix - Desert
ggplot(phoenix_desert, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Phoenix Golf Course with Desert (AZ)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Phoenix - Houses
ggplot(phoenix_houses, aes(x = date, y = ET.in)) +
  geom_point() +
  geom_line() +
  labs(title = "Phoenix Golf Course with Houses (AZ)",
       x = "Year", y = "Monthly Evapotranspiration (in)") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))




# Time Series Analysis ----

# Skenandoa Golf Course
sken_ts <- ts(skenandoa$ET.in,
              start = c(2021, 1),
              frequency = 12)
sken_dec <- decompose(sken_ts)
plot(sken_dec)
title("Skenandoa Golf Course (NY)", line = -1, outer = TRUE)

# Neighbouring Forest
forest_ts <- ts(forest$ET.in,
                start = c(2021, 1),
                frequency = 12)
forest_dec <- decompose(forest_ts)
plot(forest_dec)
title("Forest Neighbouring Skenandoa (NY)", line = -1, outer = TRUE)

# Urban - New Hartford
urban_ts <- ts(urban$ET.in,
               start = c(2021, 1),
               frequency = 12)
urban_dec <- decompose(urban_ts)
plot(urban_dec)
title("Urban Area - New Hartford (NY)", line = -1, outer = TRUE)

# Phoenix - No House, No Desert
phx_ndh_ts <- ts(phoenix_nohouse_nodesert$ET.in,
                start = c(2021, 1),
                frequency = 12)
phx_ndh_dec <- decompose(phx_ndh_ts)
plot(phx_ndh_dec)
title("Phoenix Golf - No House, No Desert (AZ)", line = -1, outer = TRUE)

# Phoenix - Desert
phx_des_ts <- ts(phoenix_desert$ET.in,
                 start = c(2021, 1),
                 frequency = 12)
phx_des_dec <- decompose(phx_des_ts)
plot(phx_des_dec)
title("Phoenix Golf - Desert (AZ)", line = -1, outer = TRUE)

# Phoenix - Houses
phx_houses_ts <- ts(phoenix_houses$ET.in,
                    start = c(2021, 1),
                    frequency = 12)
phx_houses_dec <- decompose(phx_houses_ts)
plot(phx_houses_dec)
title("Phoenix Golf - Houses (AZ)", line = -1, outer = TRUE)