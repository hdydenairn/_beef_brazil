# memoire master
# Harrison dyde-nairn

setwd("~/Documents/m2_memoire/r")

library(tidyr)
library(dplyr)
library(plm)
library(ggplot2)
library(gridExtra)

# load data & cleaning ####

# trade data
trase <- read.csv("trase.csv")
# deforestation data
mapbiomass <- read.csv("table3939.csv")


# datacleaning
trase$municipality_of_production_trase_id <- gsub("BR-", "", trase$municipality_of_production_trase_id)

# merge data sets using municipality id
merged_data <- merge(trase, mapbiomass, by.x = "municipality_of_production_trase_id", by.y = "Cód.", all.x = TRUE)

# descriptive stats 
# volume

yearly_volume_brazil <- merged_data %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019, 2020)) %>%
  group_by(year) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE))

p1 <- ggplot(yearly_volume_brazil, aes(x = year, y = total_volume)) +
  geom_line() +
  geom_point() +
  labs(title = "Volume - Brazil",
       x = "Year",
       y = "Total Volume") +
  theme_minimal()

yearly_volume_amazonia <- merged_data %>%
  group_by(year) %>%
  filter(biome == "AMAZONIA" & year %in% c(2015, 2016, 2017, 2018, 2019, 2020)) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE))

p2 <- ggplot(yearly_volume_amazonia, aes(x = year, y = total_volume)) +
  geom_line() +
  geom_point() +
  labs(title = "Volume - Amazon",
       x = "Year",
       y = "Total Volume") +
  theme_minimal()

# land use
yearly_land_use_brazil <- merged_data %>%
  filter(year %in% c(2015, 2016, 2017, 2018, 2019, 2020)) %>%
  group_by(year) %>%
  summarize(total_land_use = sum(land_use, na.rm = TRUE))

p3 <- ggplot(yearly_land_use_brazil, aes(x = year, y = total_land_use)) +
  geom_line() +
  geom_point() +
  labs(title = "Land Use - Brazil",
       x = "Year",
       y = "Total Land Use") +
  theme_minimal()

yearly_land_use_amazonia <- merged_data %>%
  filter(biome == "AMAZONIA" & year %in% c(2015, 2016, 2017, 2018, 2019, 2020)) %>%
  group_by(year) %>%
  summarize(total_land_use = sum(land_use, na.rm = TRUE))

p4 <- ggplot(yearly_land_use_amazonia, aes(x = year, y = total_land_use)) +
  geom_line() +
  geom_point() +
  labs(title = "Land Use - Amazon",
       x = "Year",
       y = "Total Land Use") +
  theme_minimal()

combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)




# filter only municiplaities in the AMAZONIA biome
amazonia <- merged_data %>%
  filter(biome == "AMAZONIA") %>%
  drop_na("Brasil.e.Município") %>%
  mutate(change_2013 = as.numeric(`X2013`) - as.numeric(`X2012`),
         change_2014 = as.numeric(`X2014`) - as.numeric(`X2013`),
         change_2015 = as.numeric(`X2015`) - as.numeric(`X2014`),
         change_2016 = as.numeric(`X2016`) - as.numeric(`X2015`),
         change_2017 = as.numeric(`X2017`) - as.numeric(`X2016`),
         change_2018 = as.numeric(`X2018`) - as.numeric(`X2017`),
         change_2019 = as.numeric(`X2019`) - as.numeric(`X2018`),
         change_2020 = as.numeric(`X2020`) - as.numeric(`X2019`),
         change_2021 = as.numeric(`X2021`) - as.numeric(`X2020`),
         change_2022 = as.numeric(`X2022`) - as.numeric(`X2021`)) %>%
  rename(levels_2012 = `X2012`) %>%
  rename(levels_2013 = `X2013`) %>%
  rename(levels_2014 = `X2014`) %>%
  rename(levels_2015 = `X2015`) %>%
  rename(levels_2016 = `X2016`) %>%
  rename(levels_2017 = `X2017`) %>%
  rename(levels_2018 = `X2018`) %>%
  rename(levels_2019 = `X2019`) %>%
  rename(levels_2020 = `X2020`) %>%
  rename(levels_2021 = `X2021`) %>%
  rename(levels_2022 = `X2022`) %>%
  rename(`2013` = change_2013) %>%
  rename(`2014` = change_2014) %>%
  rename(`2015` = change_2015) %>%
  rename(`2016` = change_2016) %>%
  rename(`2017` = change_2017) %>%
  rename(`2018` = change_2018) %>%
  rename(`2019` = change_2019) %>%
  rename(`2020` = change_2020) %>%
  rename(`2021` = change_2021) %>%
  rename(`2022` = change_2022) %>%
  mutate(year_change = NA) %>%
  mutate(year_change = case_when(
    year == 2013 ~ `2013`,
    year == 2014 ~ `2014`,
    year == 2015 ~ `2015`,
    year == 2016 ~ `2016`,
    year == 2017 ~ `2017`,
    year == 2018 ~ `2018`,
    year == 2019 ~ `2019`,
    year == 2020 ~ `2020`,
    year == 2021 ~ `2021`,
    year == 2022 ~ `2022`,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(year_change))

# export destination by country by year for each municipality ####

summary_data <- amazonia %>%
  mutate(country_of_destination = if_else(
    country_of_destination %in% c("CHINA (MAINLAND)", "CHINA (HONG KONG)"),
    "CHINA",
    country_of_destination
  )) %>%
  group_by(`Brasil.e.Município`, year, `country_of_destination`) %>%
  summarise(num_farms = n_distinct(exporter), .groups = 'drop')  %>%
  pivot_wider(names_from = `country_of_destination`, values_from = num_farms, values_fill = list(num_farms = 0))

yearly_volume <- filtered_panel_data %>%
  group_by(year) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE))

yearly_land_use <- filtered_panel_data %>%
  group_by(year) %>%
  summarize(total_land_use = sum(land_use, na.rm = TRUE))

# panel regressions ####
panel_data <- pdata.frame(amazonia, index = c("municipality_of_production_trase_id", "year"))

# FE - no covar
fe_model_no_covar <- plm(year_change ~ country_of_destination, data = panel_data, model = "within")
summary(fe_model_no_covar)

# FE - covar volume
fe_model_volume <- plm(year_change ~ country_of_destination +volume, data = panel_data, model = "within")
summary(fe_model_volume)

# FE - country of destination * year
fe_model_countryyear <- plm(year_change ~ country_of_destination * year, data = panel_data, model = "within")
summary(fe_model_countryyear)

filtered_panel_data <- panel_data %>% filter(year %in% c(2015, 2016, 2017, 2019, 2020))

aggregated_land_use <- filtered_panel_data %>%
  group_by(year, municipality_of_production, country_of_destination) %>%
  summarize(total_land_use = sum(land_use, na.rm = TRUE))

filtered_panel_data <- filtered_panel_data %>%
  mutate(year = as.character(year))

aggregated_land_use <- aggregated_land_use %>%
  mutate(year = as.character(year))

# Select the relevant columns from filtered_panel_data
year_change_data <- filtered_panel_data %>%
  select(year, municipality_of_production, year_change) %>%
  distinct()

# Join the year_change_data to the aggregated_land_use table
final_data <- aggregated_land_use %>%
  left_join(year_change_data, by = c("year", "municipality_of_production"))

# Ensure that 'year' is numeric for sorting and calculating year-on-year change
aggregated_land_use <- final_data %>%
  mutate(year = as.numeric(as.character(year)))

# Calculate the year-on-year change in total land use
land_use_changes <- aggregated_land_use %>%
  arrange(municipality_of_production, country_of_destination, year) %>%
  group_by(municipality_of_production, country_of_destination) %>%
  mutate(
    prev_total_land_use = dplyr::lag(total_land_use, n=1L), # Add this line to see lagged values
    change_in_land_use = total_land_use - prev_total_land_use
  ) %>%
  ungroup() %>%
  filter(!is.na(prev_total_land_use))

aggregated_land_use_combined <- aggregated_land_use %>%
  mutate(country_of_destination = if_else(country_of_destination %in% c("CHINA (HONG KONG)", "CHINA (MAINLAND)"), "CHINA", country_of_destination)) %>%
  group_by(year, municipality_of_production, country_of_destination) %>%
  summarize(
    total_land_use = sum(total_land_use, na.rm = TRUE),
    year_change = first(year_change)
  ) %>%
  ungroup()

aggregated_land_use_changes <- aggregated_land_use_combined %>%
  arrange(municipality_of_production, country_of_destination, year) %>%
  group_by(municipality_of_production, country_of_destination) %>%
  mutate(
    prev_total_land_use = dplyr::lag(total_land_use, n=1L), # Add this line to see lagged values
    change_in_land_use = total_land_use - prev_total_land_use
  ) %>%
  ungroup() %>%
  filter(!is.na(prev_total_land_use)) %>%
  mutate(country_CHINA = ifelse(country_of_destination == "CHINA", 1, 0))

panel_data <- pdata.frame(
  aggregated_land_use_changes,
  index = c("municipality_of_production", "year")
)

# FE - no covar
fe_model_1 <- plm(year_change ~ change_in_land_use, data = panel_data, model = "within")
summary(fe_model_1)

# Estimate the panel data model with country dummy variables
model_dummy <- plm(year_change ~ change_in_land_use + country_CHINA, data = panel_data, model = "within")

# Summary of the model
summary(model_dummy)
