#Import and Load Packages----
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

#ET Data ----
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
  rename(date = Month, ET.in = Ensemble.ET) %>% # rename columns
  mutate(date = ymd_hms(date)) %>% # fix date format
  na.omit() # remove NA entries

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


#Data Wrangling (to make it ready for comparison) of Air Temp and Precipitation ----

#Import daily NOAA Data for monthly air temp and precipitation to help with interpretation
rome.full<-read.csv("~/Desktop/ENVSTFinalData/romefullupdate.csv")
phoenix.full<-read.csv("~/Desktop/ENVSTFinalData/phoenixfullupdate.csv")

#Average temperature in F and rename
rome.full$TAVG_F <- (rome.full$TMAX + rome.full$TMIN) / 2
phoenix.full$TAVG_F <- (phoenix.full$TMAX + phoenix.full$TMIN) / 2

#Precipitation is already in inches, rename
rome.full$PRCP_in <- rome.full$PRCP
phoenix.full$PRCP_in <- phoenix.full$PRCP

#Remove NAs by making new dfs
rome.clean <- rome.full %>%
  filter(!is.na(TAVG_F)) %>%
  filter(!is.na(PRCP_in))

phoenix.clean <- phoenix.full %>%
  filter(!is.na(TAVG_F)) %>%
  filter(!is.na(PRCP_in))

#QC: Plots to check no obvious outliers

#format date as lubridate
rome.clean$DATE.F<-ymd(rome.clean$DATE)
phoenix.clean$DATE.F<-ymd(phoenix.clean$DATE)

#Match date range to ET data
rome.clean <- rome.clean %>%
  filter(DATE.F >= "2021-01-01" & DATE.F < "2026-05-01") #so that the last full month is april

phoenix.clean <- phoenix.clean %>%
  filter(DATE.F >= "2021-01-01" & DATE.F < "2026-05-01")

#Now go from daily to monthly
rome.clean$year_month <- floor_date(rome.clean$DATE.F, "month")
phoenix.clean$year_month <- floor_date(phoenix.clean$DATE.F, "month")

rome.monthly <- rome.clean %>%
  group_by(year_month) %>%
  summarise( mean.temp = mean(TAVG_F),
    total.prcp = sum(PRCP_in))

phoenix.monthly <- phoenix.clean %>%
  group_by(year_month) %>%
  summarise( mean.temp = mean(TAVG_F),
    total.prcp = sum(PRCP_in))

#QC: Plot to check if any outlier
plot(phoenix.monthly$mean.temp)
plot(phoenix.monthly$total.prcp)
plot(rome.monthly$mean.temp)
plot(rome.monthly$total.prcp)

#Rome
ggplot(rome.monthly) +
  aes(x = year_month, y = mean.temp) +
  geom_line() +
  labs(
    title = "Monthly Average Temperature (Rome, NY)",
    x = "Month",
    y = "Temperature (°F)")

ggplot(rome.monthly) +
  aes(x = year_month, y = total.prcp) +
  geom_line() +
  labs(
    title = "Monthly Total Precipitation (Rome, NY)",
    x = "Month",
    y = "Precipitation (inches)")

#Phoenix
ggplot(phoenix.monthly) +
  aes(x = year_month, y = mean.temp) +
  geom_line() +
  labs(
    title = "Monthly Average Temperature (Phoenix, AZ)",
    x = "Month",
    y = "Temperature (°F)"
  )

ggplot(phoenix.monthly) +
  aes(x = year_month, y = total.prcp) +
  geom_line() +
  labs(
    title = "Monthly Total Precipitation (Phoenix, AZ)",
    x = "Month",
    y = "Precipitation (inches)"
  )
#Everything seems to be right - no outliers

#Make Data Frame for each region decomposition----
#Cite: Help from Professor Kropp

# 1. Skenandoa Golf Course
sken.df <- data.frame(
  year = c(rep(seq(2021, 2025), each = 12), 2026, 2026, 2026),
  month = c(rep(seq(1, 12), times = 5), 1, 2, 3),
  seasonal = sken_dec$seasonal,
  observed = sken_dec$x,
  trend = sken_dec$trend,
  random = sken_dec$random
)

sken.df$name <- rep("Skenandoa (NY)", nrow(sken.df))
sken.df$location <- rep("NY", nrow(sken.df))

# 2. Neighbouring Forest
forest.df <- data.frame(
  year = c(rep(seq(2021, 2025), each = 12), 2026, 2026, 2026),
  month = c(rep(seq(1, 12), times = 5), 1, 2, 3),
  seasonal = forest_dec$seasonal,
  observed = forest_dec$x,
  trend = forest_dec$trend,
  random = forest_dec$random
)

forest.df$name <- rep("Neighbouring Forest", nrow(forest.df))
forest.df$location <- rep("NY", nrow(forest.df))

# 3. Urban Area - New Hartford
urban.df <- data.frame(
  year = c(rep(seq(2021, 2025), each = 12), 2026, 2026, 2026),
  month = c(rep(seq(1, 12), times = 5), 1, 2, 3),
  seasonal = urban_dec$seasonal,
  observed = urban_dec$x,
  trend = urban_dec$trend,
  random = urban_dec$random
)

urban.df$name <- rep("Urban Area - New Hartford", nrow(urban.df))
urban.df$location <- rep("NY", nrow(urban.df))

# 4. Phoenix Golf Course - No House, No Desert
phx_ndh.df <- data.frame(
  year = c(rep(seq(2021, 2025), each = 12), 2026, 2026, 2026),
  month = c(rep(seq(1, 12), times = 5), 1, 2, 3),
  seasonal = phx_ndh_dec$seasonal,
  observed = phx_ndh_dec$x,
  trend = phx_ndh_dec$trend,
  random = phx_ndh_dec$random
)

phx_ndh.df$name <- rep("Phoenix Golf - No House, No Desert", nrow(phx_ndh.df))
phx_ndh.df$location <- rep("AZ", nrow(phx_ndh.df))

# 5. Phoenix Golf Course with Houses

phx_houses_dec$seasonal[1:10] 
houses.df<-data.frame(year=c(rep(seq(2021,2025),each=12),2026,2026,2026,2026),
                      month=c(rep(seq(1,12),times=5),1,2,3,4),
                      seasonal=phx_houses_dec$seasonal,
                      observed=phx_houses_dec$x, 
                      trend=phx_houses_dec$trend,
                      random=phx_houses_dec$random)
houses.df$name=rep("Phoenix Golf - Houses",nrow(houses.df))
houses.df$location=rep("AZ",nrow(houses.df))

# 6. Phoenix Golf Course - Desert
phx_des.df <- data.frame(
  year = c(rep(seq(2021, 2025), each = 12), 2026, 2026, 2026),
  month = c(rep(seq(1, 12), times = 5), 1, 2, 3),
  seasonal = phx_des_dec$seasonal,
  observed = phx_des_dec$x,
  trend = phx_des_dec$trend,
  random = phx_des_dec$random
)

phx_des.df$name <- rep("Phoenix Golf - Desert", nrow(phx_des.df))
phx_des.df$location <- rep("AZ", nrow(phx_des.df))

#Combine climate data and format to make it easy to refer to in analysis of regions
rome.monthly$location <- "NY"
phoenix.monthly$location <- "AZ"
rome.monthly$year <- year(rome.monthly$year_month)
rome.monthly$month <- month(rome.monthly$year_month)

phoenix.monthly$year <- year(phoenix.monthly$year_month)
phoenix.monthly$month <- month(phoenix.monthly$year_month)
climate.monthly <- rbind(rome.monthly, phoenix.monthly)

#Combine data for each analysis:
#NY Comparison:


#AZ Comparison


#NY vs AZ Comparison




#Comparative Data Analysis----
#Summary stat - mean
avg_sken_trans<-mean(sken_dec$x)
avg_forest_trans<-mean(forest_dec$x)
avg_urban_trans<-mean(urban_dec$x)
avg_phx_trans<-mean(phx_ndh_dec$x)
avg_phx_des_trans<-mean(phx_des_dec$x)
avg_phx_house_trans<-mean(phx_houses_dec$x)

#Golf Course - Sken vs Neighbouring Forest and Ubran Area (NY)

#Sken has mean of 2.36 inches, Forest has mean of 2.8 inches, Urban area has mean of 1.56 inches
#Sken hence uses more water than urban areas but less than forests, showing no obvious sign of needing alot of water

#Plot trend, seasonal and random patterns for the three on one graph



#Golf Course vs Golf Course with Houses vs Golf Course with Desert (AZ)
#Mean comparison
#Decomposition of each element on one graph



#Golf Course in NY vs Golf Course (No House, No Desert) in AZ 
#Mean comparison
#Decomposition of each element on one graph
#Use air temperature and precipitation data to make sense of any randomness








