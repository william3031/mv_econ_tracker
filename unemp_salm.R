# unemployment data
# https://www.employment.gov.au/small-area-labour-markets-publication-0

# libraries
pacman::p_load(tidyverse, readxl, lubridate, janitor, tsibble, sf)

#what is the current salm month???
current_salm_month <- "2019-12" #write it in this format "YYYY-MM"

#import files  sa2 - change dates ####
sa2data <- read_excel("data_in/SALM Smoothed SA2 Datafiles (ASGS 2016) - December quarter 2019.xlsx",
                      sheet = "Smoothed SA2 unemployment rate", range = cell_rows(4:2176)) %>% 
  clean_names() %>% 
  rename(sa2_code = sa2_code_2016_asgs, sa2_name = statistical_area_level_2_sa2_2016_asgs)

sa2_col_names <- colnames(sa2data)

salm_current_numeric <- sa2_col_names[length(sa2_col_names)] %>% 
  str_remove(., "x")  %>% 
  as.numeric(.)

salm_current_month <- as.Date(salm_current_numeric, origin = "1899-12-30")
  
#reshape data sa2 unemp ###########################################################
sa2_vic_tidy <- sa2data %>% 
  pivot_longer(cols = -(sa2_name:sa2_code), names_to = "date", values_to = "values")  %>%
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = as.numeric(date)) %>% 
  mutate(date = as.Date(date, origin = "1899-12-30")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  filter(sa2_code >= 200000000 & sa2_code <= 299999999)

sa2_vic_current <- sa2_vic_tidy %>% 
  filter(date == salm_current_month) %>% 
  select(-sa2_code) %>% 
  rename(rate = values)
write_csv(sa2_vic_current, "app_data/salm_unemp_current_sa2.csv")

# spatial data - already simplified
sa2_greater <- st_read("data_in/shp/sa2_2016_gmel.shp") %>% 
  select(-sa2_code) %>% 
  clean_names() 

# join for app
unemp_rate_map_join <- left_join(sa2_greater, sa2_vic_current) 
  

#import files  lga - change dates ####
lgadata <- read_excel("data_in/SALM Smoothed LGA Datafiles (ASGS 2019) - December quarter 2019.xlsx",
                      sheet = "Smoothed LGA unemployment rates", range = cell_rows(4:546)) %>% 
  clean_names() %>% 
  rename(region = local_government_area_lga_2019_asgs, lga_code = lga_code_2019_asgs)

lga_salm_col_names <- colnames(lgadata)

lgamv <- lgadata %>% 
  filter(region ==  "Moonee Valley (C)") %>% 
  select (-lga_code)
lgamv

lgamv_tidy <- lgamv %>% 
  gather(date_ex, unemployment_rate, lga_salm_col_names[3]: length(sa2mv)) %>%
  mutate(date_ex = str_remove(date_ex, "x")) %>% 
  mutate(mon_yr = excel_numeric_to_date(as.numeric(as.character((date_ex))), date_system = "modern")) %>% 
  mutate(mon_yr = yearmonth(mon_yr))%>% 
  mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
  mutate(unemployment_rate = round(unemployment_rate, digits =1)) %>% 
  mutate(type = "LGA")

sa2mv <- sa2data %>% 
  filter(region %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                       "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore")) %>% 
  select (-sa2_code)
sa2mv


merged_unemp <- bind_rows(sa2mv_tidy, lgamv_tidy) %>% 
  arrange(desc(date_ex), desc(type), region)

# add greater melbourne and victoria
# add labour force and unemployed - put in to longer table possible need another wider one
# change the date format - no need for mon_yr ????
