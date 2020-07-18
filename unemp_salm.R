# unemployment data
# https://www.employment.gov.au/small-area-labour-markets-publication-0

# libraries
pacman::p_load(tidyverse, readxl, lubridate, janitor, zoo, tsibble)

#what is the current salm month???
current_salm_month <- "2019-12" #write it in this format "YYYY-MM"

#import files  sa2 - change dates ####
sa2data <- read_excel("data_in/SALM Smoothed SA2 Datafiles (ASGS 2016) - December quarter 2019.xlsx",
                      sheet = "Smoothed SA2 unemployment rate", range = cell_rows(4:2176)) %>% 
  clean_names() %>% 
  rename(sa2_code = sa2_code_2016_asgs, region = statistical_area_level_2_sa2_2016_asgs)

sa2mv <- sa2data %>% 
  filter(region %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                       "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore")) %>% 
  select (-sa2_code)
sa2mv

#import files  lga - change dates ####
lgadata <- read_excel("data_in/SALM Smoothed LGA Datafiles (ASGS 2019) - December quarter 2019.xlsx",
                      sheet = "Smoothed LGA unemployment rates", range = cell_rows(4:546)) %>% 
  clean_names() %>% 
  rename(region = local_government_area_lga_2019_asgs, lga_code = lga_code_2019_asgs)
  
salm_col_names <- colnames(lgadata)

lgamv <- lgadata %>% 
  filter(region ==  "Moonee Valley (C)") %>% 
  select (-lga_code)
lgamv

#reshape data
sa2mv_tidy <- sa2mv %>% 
  gather(date_ex, unemployment_rate, salm_col_names[3]: length(sa2mv)) %>%
  mutate(date_ex = str_remove(date_ex, "x")) %>% 
  mutate(mon_yr = excel_numeric_to_date(as.numeric(as.character((date_ex))), date_system = "modern")) %>% 
  mutate(mon_yr = yearmonth(mon_yr))%>% 
  mutate(type = "SA2") %>% 
  mutate(unemployment_rate = as.numeric(unemployment_rate))

lgamv_tidy <- lgamv %>% 
  gather(date_ex, unemployment_rate, salm_col_names[3]: length(sa2mv)) %>%
  mutate(date_ex = str_remove(date_ex, "x")) %>% 
  mutate(mon_yr = excel_numeric_to_date(as.numeric(as.character((date_ex))), date_system = "modern")) %>% 
  mutate(mon_yr = yearmonth(mon_yr))%>% 
  mutate(unemployment_rate = as.numeric(unemployment_rate)) %>% 
  mutate(unemployment_rate = round(unemployment_rate, digits =1)) %>% 
  mutate(type = "LGA")

merged_unemp <- bind_rows(sa2mv_tidy, lgamv_tidy) %>% 
  arrange(desc(date_ex), desc(type), region)

# add greater melbourne and victoria
# add labour force and unemployed - put in to longer table possible need another wider one
# change the date format - no need for mon_yr ????
