# Load the tidyverse package and required libraries
library(tidyverse)

# Read the CSV data sets into data frames
KAG_energydata_complete <- read_csv(
  "Energy Usage Data/KAG_energydata_complete.csv")
liander_2019 <- read_csv("Energy Usage Data/liander_electricity_01012019.csv")
liander_2020 <- read_csv("Energy Usage Data/liander_electricity_01012020.csv")
AEP_hourly <- read_csv("Energy Usage Data/AEP_hourly.csv")
PJME_hourly <- read_csv("Energy Usage Data/PJME_hourly.csv")
PJMW_hourly <- read_csv("Energy Usage Data/PJMW_hourly.csv")
all_energy_statistics <- read_csv("Energy Usage Data/all_energy_statistics.csv")
world_energy_consumption <- read_csv(
  "Energy Usage Data/World Energy Consumption.csv")

# Filter relevant columns
KAG_energydata_filtered <- KAG_energydata_complete %>% 
  rename(Date = date) %>% 
  mutate(EnergyConsumption = Appliances + lights) %>% 
  select(Date, EnergyConsumption)

# Aggregate relevant columns
liander_2019_aggregated <- liander_2019 %>% 
  group_by(city, zipcode_from, zipcode_to) %>% 
  summarise(TotalAnnualConsume = sum(annual_consume), 
            AvgSmartMeterPerc = mean(smartmeter_perc))
liander_2020_aggregate <- liander_2020 %>% 
  group_by(city, zipcode_from, zipcode_to) %>% 
  summarise(TotalAnnualConsume = sum(annual_consume), 
            AvgSmartMeterPerc = mean(smartmeter_perc))

# Daily sums of energy consumption aggregated for KAG_energydata_complete
KAG_energy_daily_sum <- KAG_energydata_complete %>% 
  group_by(date) %>% 
  summarise(DailyApplianceUse = sum(Appliances))

# Aggregation for hourly data
hourly_merged <- inner_join(AEP_hourly, PJME_hourly, by = c("Date", "Hour"), 
                            suffix = c("_AEP", "_PJME")) %>% 
  inner_join(PJMW_hourly, by - c("Date", "Hour")) %>% 
  mutate(HourlyAvg = (
    Consumption_AEP + Consumption_PJME + Consumption_PJMW) / 3)
