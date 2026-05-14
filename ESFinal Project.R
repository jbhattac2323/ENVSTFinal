#Import and Load Packages----
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

#Load ET Data ----
skenandoa <- read.csv("~/Desktop/ENVSTFinalData/SkenETdata.csv")
forest <- read.csv("~/Desktop/ENVSTFinalData/NeighbouringForestofSken.csv")
urban <- read.csv("~/Desktop/ENVSTFinalData/NewHartfordET.csv")
phoenix_nohouse_nodesert <- read.csv("~/Desktop/ENVSTFinalData/PhoenixGolfcoursenohousenodesert.csv")
phoenix_desert <- read.csv("~/Desktop/ENVSTFinalData/PhoenixGolfCoursewNohouses.csv")
phoenix_houses <- read.csv("~/Desktop/ENVSTFinalData/golfcoursewHousesPhoenix.csv")

head(skenandoa)

#Data Wrangling of ET Data----
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


#Data Wrangling of Air Temp and Precipitation ----

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
    y = "Temperature (°F)")+
  theme_classic()

ggplot(rome.monthly) +
  aes(x = year_month, y = total.prcp) +
  geom_line() +
  labs(
    title = "Monthly Total Precipitation (Rome, NY)",
    x = "Month",
    y = "Precipitation (inches)")+
  theme_classic()

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

#Wrangling: Data Frame for each region decomposition----
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

sken.df$Name <- rep("Skenandoa (NY)", nrow(sken.df))
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

forest.df$Name <- rep("Neighbouring Forest", nrow(forest.df))
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

urban.df$Name <- rep("Urban Area - New Hartford", nrow(urban.df))
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

phx_ndh.df$Name <- rep("Phoenix Golf - No House, No Desert", nrow(phx_ndh.df))
phx_ndh.df$location <- rep("AZ", nrow(phx_ndh.df))

# 5. Phoenix Golf Course with Houses

phx_houses_dec$seasonal[1:10] 
houses.df<-data.frame(year=c(rep(seq(2021,2025),each=12),2026,2026,2026,2026),
                      month=c(rep(seq(1,12),times=5),1,2,3,4),
                      seasonal=phx_houses_dec$seasonal,
                      observed=phx_houses_dec$x, 
                      trend=phx_houses_dec$trend,
                      random=phx_houses_dec$random)
houses.df$Name=rep("Phoenix Golf - Houses",nrow(houses.df))
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

phx_des.df$Name <- rep("Phoenix Golf - Desert", nrow(phx_des.df))
phx_des.df$location <- rep("AZ", nrow(phx_des.df))

#Combine climate data and format to make it easy to refer to in analysis of regions
rome.monthly$location <- "NY"
phoenix.monthly$location <- "AZ"
rome.monthly$year <- year(rome.monthly$year_month)
rome.monthly$month <- month(rome.monthly$year_month)

phoenix.monthly$year <- year(phoenix.monthly$year_month)
phoenix.monthly$month <- month(phoenix.monthly$year_month)
climate.monthly <- rbind(rome.monthly, phoenix.monthly)

#Convert decomposition columns from time series to numeric
# Skenandoa
sken.df$seasonal <- as.numeric(sken.df$seasonal)
sken.df$observed <- as.numeric(sken.df$observed)
sken.df$trend <- as.numeric(sken.df$trend)
sken.df$random <- as.numeric(sken.df$random)

# Forest
forest.df$seasonal <- as.numeric(forest.df$seasonal)
forest.df$observed <- as.numeric(forest.df$observed)
forest.df$trend <- as.numeric(forest.df$trend)
forest.df$random <- as.numeric(forest.df$random)

# Urban
urban.df$seasonal <- as.numeric(urban.df$seasonal)
urban.df$observed <- as.numeric(urban.df$observed)
urban.df$trend <- as.numeric(urban.df$trend)
urban.df$random <- as.numeric(urban.df$random)

# Phoenix - No House, No Desert
phx_ndh.df$seasonal <- as.numeric(phx_ndh.df$seasonal)
phx_ndh.df$observed <- as.numeric(phx_ndh.df$observed)
phx_ndh.df$trend <- as.numeric(phx_ndh.df$trend)
phx_ndh.df$random <- as.numeric(phx_ndh.df$random)

# Phoenix - Houses
houses.df$seasonal <- as.numeric(houses.df$seasonal)
houses.df$observed <- as.numeric(houses.df$observed)
houses.df$trend <- as.numeric(houses.df$trend)
houses.df$random <- as.numeric(houses.df$random)

# Phoenix - Desert
phx_des.df$seasonal <- as.numeric(phx_des.df$seasonal)
phx_des.df$observed <- as.numeric(phx_des.df$observed)
phx_des.df$trend <- as.numeric(phx_des.df$trend)
phx_des.df$random <- as.numeric(phx_des.df$random)


#Combine data + Visualistion for each analysis----
#1. NY Comparison----
ny.dec.df <- rbind(sken.df, urban.df, forest.df)
ny.dec.df$date <- make_date( # Creating date from year and month to be able to plot
  year = ny.dec.df$year,
  month = ny.dec.df$month,
  day = 1 #arbitrary / how it was before
)

#NY Observed ET Comparison
ggplot(data = ny.dec.df,
       aes(x = date, y = observed, color = Name)) +
  geom_line() +
  labs(
    title = "Observed Monthly ET: NY Sites",
    x = "Year",
    y = "Observed Monthly ET (in)"
  ) +
  theme_classic()

#NY Trend ET Comparison
ggplot(data = ny.dec.df,
       aes(x = date, y = trend, color = Name)) +
  geom_line() +
  labs(
    title = "Trend Component of ET: NY Sites",
    x = "Year",
    y = "Trend Component of ET (in)"
  ) +
  theme_classic()

#NY Seasonal ET Comparison
ggplot(data = ny.dec.df,
       aes(x = date, y = seasonal, color = Name)) +
  geom_line() +
  labs(
    title = "Seasonal Component of ET: NY Sites",
    x = "Year",
    y = "Seasonal Component of ET (in)"
  ) +
  theme_classic()

#NY Random ET Comparison
ggplot(data = ny.dec.df,
       aes(x = date, y = random, color = Name)) +
  geom_line() +
  labs(
    title = "Random Component of ET: NY Sites",
    x = "Year",
    y = "Random Component of ET (in)"
  ) +
  theme_classic()

#2. AZ Comparison----
az.dec.df<-rbind(houses.df,phx_des.df,phx_ndh.df)
az.dec.df$date <- make_date(
  year = az.dec.df$year,
  month = az.dec.df$month,
  day = 1
)

#AZ Observed ET Comparison
ggplot(data = az.dec.df,
       aes(x = date, y = observed, color = Name)) +
  geom_line() +
  labs(
    title = "Observed Monthly ET: AZ Sites",
    x = "Year",
    y = "Observed Monthly ET (in)"
  ) +
  theme_classic()

#AZ Trend ET Comparison
ggplot(data = az.dec.df,
       aes(x = date, y = trend, color = Name)) +
  geom_line() +
  labs(
    title = "Trend Component of ET: AZ Sites",
    x = "Year",
    y = "Trend Component of ET (in)"
  ) +
  theme_classic()

#AZ Seasonal ET Comparison
ggplot(data = az.dec.df,
       aes(x = date, y = seasonal, color = Name)) +
  geom_line() +
  labs(
    title = "Seasonal Component of ET: AZ Sites",
    x = "Year",
    y = "Seasonal Component of ET (in)"
  ) +
  theme_classic()

#AZ Random ET Comparison
ggplot(data = az.dec.df,
       aes(x = date, y = random, color = Name)) +
  geom_line() +
  labs(
    title = "Random Component of ET: AZ Sites",
    x = "Year",
    y = "Random Component of ET (in)"
  ) +
  theme_classic()

#3. NY vs AZ Comparison----
ny.az.comp.df<-rbind(sken.df,phx_ndh.df)
ny.az.comp.df$date <- make_date(
  year = ny.az.comp.df$year,
  month = ny.az.comp.df$month,
  day = 1
)
#Observed ET Comparison
ggplot(data = ny.az.comp.df,
       aes(x = date, y = observed, color = Name)) +
  geom_line() +
  labs(
    title = "Observed Monthly ET: NY Golf Course vs AZ Golf Course",
    x = "Year",
    y = "Observed Monthly ET (in)"
  ) +
  theme_classic()

#Trend ET Comparison
ggplot(data = ny.az.comp.df,
       aes(x = date, y = trend, color = Name)) +
  geom_line() +
  labs(
    title = "Trend Component of ET: NY Golf Course vs AZ Golf Course",
    x = "Year",
    y = "Trend Component of ET (in)"
  ) +
  theme_classic()

#Seasonal ET Comparison
ggplot(data = ny.az.comp.df,
       aes(x = date, y = seasonal, color = Name)) +
  geom_line() +
  labs(
    title = "Seasonal Component of ET: NY Golf Course vs AZ Golf Course",
    x = "Year",
    y = "Seasonal Component of ET (in)"
  ) +
  theme_classic()

#Random ET Comparison
ggplot(data = ny.az.comp.df,
       aes(x = date, y = random, color = Name)) +
  geom_line() +
  labs(
    title = "Random Component of ET: NY Golf Course vs AZ Golf Course",
    x = "Year",
    y = "Random Component of ET (in)"
  ) +
  theme_classic()

#Summary stat -----
#Investigate Linear Relationship between ET and Temperature
all.dec.df <- rbind(
  sken.df,
  urban.df,
  forest.df,
  houses.df,
  phx_des.df,
  phx_ndh.df
)

all.dec.df$date <- make_date(
  year = all.dec.df$year,
  month = all.dec.df$month,
  day = 1
)


et.climate.df <- left_join(
  all.dec.df,
  climate.monthly,
  by = c("year", "month", "location")
)

#temperature
ggplot(data = et.climate.df,
       aes(x = mean.temp, y = random, color = location, shape = Name)) +
  geom_point() +
  labs(
    title = "ET Random Component vs Monthly Average Temperature",
    x = "Monthly Average Temperature (°F)",
    y = "Random Component of ET (in)"
  ) +
  theme_classic()

cor(et.climate.df$random, et.climate.df$mean.temp, use = "complete.obs")
#cite https://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

#precipitation
ggplot(data = et.climate.df,
       aes(x = total.prcp, y = random, color = location, shape = Name)) +
  geom_point() +
  labs(
    title = "ET Random Component vs Monthly Total Precipitation",
    x = "Monthly Total Precipitation (inches)",
    y = "Random Component of ET (in)"
  ) +
  theme_classic()
cor(et.climate.df$random, et.climate.df$total.prcp, use = "complete.obs")

#mean
avg_sken_trans <- mean(sken_dec$x, na.rm = TRUE)
avg_forest_trans <- mean(forest_dec$x, na.rm = TRUE)
avg_urban_trans <- mean(urban_dec$x, na.rm = TRUE)
avg_phx_trans <- mean(phx_ndh_dec$x, na.rm = TRUE)
avg_phx_des_trans <- mean(phx_des_dec$x, na.rm = TRUE)
avg_phx_house_trans <- mean(phx_houses_dec$x, na.rm = TRUE)

#standard deviation
sd_sken_trans <- sd(sken_dec$x, na.rm = TRUE)
sd_forest_trans <- sd(forest_dec$x, na.rm = TRUE)
sd_urban_trans <- sd(urban_dec$x, na.rm = TRUE)
sd_phx_trans <- sd(phx_ndh_dec$x, na.rm = TRUE)
sd_phx_des_trans <- sd(phx_des_dec$x, na.rm = TRUE)
sd_phx_house_trans <- sd(phx_houses_dec$x, na.rm = TRUE)

#Golf Course - Sken vs Neighbouring Forest and Ubran Area (NY)

#Sken has mean of 2.36 inches, Forest has mean of 2.8 inches, Urban area has mean of 1.56 inches
#Sken hence uses more water than urban areas but less than forests, showing no obvious sign of needing alot of water

#Golf Course vs Golf Course with Houses vs Golf Course with Desert (AZ)


#Golf Course in NY vs Golf Course (No House, No Desert) in AZ 

#Use air temperature and precipitation data to make sense of any randomness








