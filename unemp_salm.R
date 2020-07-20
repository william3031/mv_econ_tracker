# unemployment data
# https://www.employment.gov.au/small-area-labour-markets-publication-0

# libraries
pacman::p_load(tidyverse, readxl, lubridate, janitor, tsibble, sf)

sa2_data_path <- "data_in/SALM Smoothed SA2 Datafiles (ASGS 2016) - December quarter 2019.xlsx"
lga_data_path <- "data_in/SALM Smoothed LGA Datafiles (ASGS 2019) - December quarter 2019.xlsx"

#import files  sa2 unemployment
sa2_data_unemp_rate <- read_excel(sa2_data_path,
                      sheet = "Smoothed SA2 unemployment rate", range = cell_rows(4:2176)) %>% 
  clean_names() %>% 
  rename(sa2_code = sa2_code_2016_asgs, sa2_name = statistical_area_level_2_sa2_2016_asgs)

sa2_data_unemployment <- read_excel(sa2_data_path,
                                  sheet = "Smoothed SA2 unemployment", range = cell_rows(4:2176)) %>% 
  clean_names() %>% 
  rename(sa2_code = sa2_code_2016_asgs, sa2_name = statistical_area_level_2_sa2_2016_asgs)

sa2_data_labour_force <- read_excel(sa2_data_path,
                                    sheet = "Smoothed SA2 labour force", range = cell_rows(4:2176)) %>% 
  clean_names() %>% 
  rename(sa2_code = sa2_code_2016_asgs, sa2_name = statistical_area_level_2_sa2_2016_asgs)

sa2_col_names <- colnames(sa2_data_unemp_rate)

salm_current_numeric <- sa2_col_names[length(sa2_col_names)] %>% 
  str_remove(., "x")  %>% 
  as.numeric(.)

salm_current_month <- as.Date(salm_current_numeric, origin = "1899-12-30")

# import lga data - don't need the unemployment rate as it is calculated
lga_data_unemployment <- read_excel(lga_data_path,
                                  sheet = "Smoothed LGA unemployment", range = cell_rows(4:546)) %>% 
  clean_names() %>% 
  rename(lga_code = lga_code_2019_asgs, lga_name = local_government_area_lga_2019_asgs)

lga_data_labour_force <- read_excel(lga_data_path,
                                    sheet = "Smoothed LGA labour force", range = cell_rows(4:546)) %>% 
  clean_names() %>% 
  rename(lga_code = lga_code_2019_asgs, lga_name = local_government_area_lga_2019_asgs)

# spatial data - already simplified #############################
sa2_greater <- st_read("data_in/shp/sa2_2016_gmel.shp") %>% 
  clean_names() 

gr_melb_sa2_9digit_list <- sa2_greater %>% 
  st_set_geometry(NULL) %>% 
  select(sa2_code) %>% 
  pull()
  
#reshape data sa2 unemp rate ###########################################################
sa2_vic_tidy_unemp_rate <- sa2_data_unemp_rate %>% 
  pivot_longer(cols = -(sa2_name:sa2_code), names_to = "date", values_to = "values")  %>%
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  filter(sa2_code >= 200000000 & sa2_code <= 299999999)

sa2_vic_current_unemp_rate <- sa2_vic_tidy_unemp_rate %>% 
  filter(date == salm_current_month) %>% 
  select(-sa2_code) %>% 
  rename(rate = values)
write_csv(sa2_vic_current_unemp_rate, "app_data/salm_unemp_rate_current_sa2.csv")

# spatial data - already simplified
sa2_greater <- st_read("data_in/shp/sa2_2016_gmel.shp") %>% 
  clean_names() 

# join for app
unemp_rate_map_join <- left_join(sa2_greater, sa2_vic_current_unemp_rate) 

# for unemployment and labour force sa2 vic ##################################
sa2_vic_tidy_unemployment <- sa2_data_unemployment %>% 
  pivot_longer(cols = -(sa2_name:sa2_code), names_to = "date", values_to = "values")  %>%
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  filter(sa2_code >= 200000000 & sa2_code <= 299999999) %>% 
  rename(unemployed = values)

sa2_vic_tidy_labour_force <- sa2_data_labour_force %>% 
  pivot_longer(cols = -(sa2_name:sa2_code), names_to = "date", values_to = "values")  %>%
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  filter(sa2_code >= 200000000 & sa2_code <= 299999999) %>% 
  rename(labour_force = values)

sa2_merge_data <- sa2_vic_tidy_unemp_rate %>% 
  rename(unemp_rate = values) %>% 
  bind_cols(sa2_vic_tidy_unemployment[length(sa2_vic_tidy_unemployment)]) %>% 
  bind_cols(sa2_vic_tidy_labour_force[length(sa2_vic_tidy_labour_force)]) 

sa2_merge_data_gm <- sa2_merge_data %>% 
  filter(sa2_code %in% gr_melb_sa2_9digit_list) %>% 
  group_by(date) %>% 
  select(-unemp_rate) %>% 
  replace_na(list(unemployed = 0, labour_force = 0)) %>% 
  summarise(unemployed = sum(unemployed), labour_force = sum(labour_force)) %>% 
  mutate(region = "Greater Melbourne", unemp_rate = round(unemployed/labour_force*100, 1)) %>% 
  select(region, date, unemp_rate, unemployed, labour_force)

# for unemployment and labour force lga vic ##################################
lga_mv_tidy_unemployment <- lga_data_unemployment %>% 
  pivot_longer(cols = -(lga_name:lga_code), names_to = "date", values_to = "values")  %>%
  filter(lga_name == "Moonee Valley (C)") %>% 
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  rename(unemployed = values)

lga_mv_tidy_labour_force <- lga_data_labour_force %>% 
  pivot_longer(cols = -(lga_name:lga_code), names_to = "date", values_to = "values")  %>%
  filter(lga_name == "Moonee Valley (C)") %>% 
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  rename(labour_force = values)

lga_mv_merge_data <- lga_mv_tidy_unemployment %>% 
  bind_cols(lga_mv_tidy_labour_force[length(lga_mv_tidy_labour_force)]) %>% 
  mutate(region = "City of Moonee Valley", unemp_rate = round(unemployed/labour_force*100, 1)) %>% 
  select(region, date, unemp_rate, unemployed, labour_force)

# put it all together
salm_large <- sa2_merge_data %>% 
  filter(sa2_name %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                         "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore")) %>% 
  select(-sa2_code) %>% 
  rename(region = sa2_name) %>% 
  bind_rows(lga_mv_merge_data) %>% 
  bind_rows(sa2_merge_data_gm) 

# to export
salm_data_list <- c("Unemployment rate %", "No. of unemployed", "Labour force")

salm_chart_data <- salm_large %>% 
  rename(`Unemployment rate %` = unemp_rate, `No. of unemployed` = unemployed, `Labour force` = labour_force) %>% 
  pivot_longer(-(region:date), names_to = "data_type", values_to = "values")
write_csv(salm_chart_data, "app_data/salm_chart_data.csv")

salm_table_data <- salm_large %>% 
  filter(date == salm_current_month) %>% 
  mutate(unemployed = format(unemployed, big.mark = ","), labour_force = format(labour_force, big.mark = ",")) %>% 
  rename(Region = region, `Unemployment rate %` = unemp_rate, `No. of unemployed` = unemployed, `Labour force` = labour_force) %>% 
  select(-date) 
write_csv(salm_table_data, "app_data/salm_table_data.csv")

