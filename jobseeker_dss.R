# jobseeker and youth allowance data
# https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile

# libraries
pacman::p_load(tidyverse, readxl, janitor, scales, sf, rmapshaper, lubridate, RColorBrewer)

# don't use purrr on this as the files are the same, cant' work out month - note the dates in the excel file
data_mar20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-march-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-03-27")

data_apr20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-april-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-04-24")

data_may20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-may-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-05-29")

data_jun20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-june-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-06-26")

## simplication of the sa2_file
#sa2_initial <- st_read("data_in/shp/sa2_2016_gmelb.shp") %>% # already deleted, but is an export from gisdb
#  clean_names() %>% 
#  select(sa2_code = sa2_mainco, sa2_5digit, sa2_name = sa2_name_2) %>% 
#  ms_simplify()
#st_write(sa2_initial, "data_in/shp/sa2_2016_gmel.shp", delete_layer = TRUE)

# spatial data - already simplified #############################
sa2_greater <- st_read("data_in/shp/sa2_2016_gmel.shp") %>% 
  clean_names() 

gr_melb_sa2_5digit_list <- sa2_greater %>% 
  st_set_geometry(NULL) %>% 
  select(sa2_5digit) %>% 
  pull()

### merge it all
jobseeker_merge <- bind_rows(data_mar20, data_apr20, data_may20, data_jun20) %>% 
  mutate(job_seeker_payment = parse_number(job_seeker_payment), youth_allowance_other = parse_number(youth_allowance_other)) %>% 
  mutate(month = ymd(month)) %>%
  mutate(total = job_seeker_payment + youth_allowance_other) %>% 
  filter((sa2 >= 20000 & sa2 <= 29999))

jobseeker_merge_sa2 <- jobseeker_merge %>% 
  filter(sa2_name %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                         "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore")) %>% 
  select(-sa2) %>% 
  rename(region = sa2_name)

# these for the app table part 1 of 2
jobseeker_merge_mv <- jobseeker_merge_sa2 %>% 
  group_by(month) %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "City of Moonee Valley")

jobseeker_merge_gm <- jobseeker_merge %>% 
  filter(sa2 %in% gr_melb_sa2_5digit_list) %>% 
  group_by(month)  %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "Greater Melbourne")

jobseeker_merge_vic <- jobseeker_merge %>% 
  group_by(month)  %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "Victoria")

jobseeker_all <- bind_rows(jobseeker_merge_sa2, jobseeker_merge_mv, jobseeker_merge_gm, jobseeker_merge_vic) 

# add 15-64 population proportion ########################################################################

# asgs
asgs_col_names <- c("S/T code", "S/T name", "GCCSA code", "GCCSA name", "SA4 code", "SA4 name", "SA3 code", "SA3 name", "SA2 code", "SA2 name", "Age0-4", "Age5–9", "Age10–14", "Age15–19", "Age20–24", "Age25–29", "Age30–34", "Age35–39", "Age40–44", "Age45–49", "Age50–54", "Age55–59", "Age60–64", "Age65–69", "Age70–74", "Age75–79", "Age80–84", "Age85 and over", "Total Persons")

asgs_age <- read_excel("data_in/32350ds0001_asgs2016_2018.xls", sheet = "Table 3", skip = 9, col_names = asgs_col_names) %>% 
  clean_names() %>% 
  filter(s_t_name == "Victoria") 

# all vic sa2s 15-64
vic_sa2_1564 <- asgs_age %>% 
  select(-(s_t_code:sa2_code)) %>% 
  select(sa2_name, age15_19:age60_64) %>% 
  pivot_longer(-sa2_name, names_to = "age_groups", values_to = "count") %>% 
  group_by(sa2_name) %>% 
  summarise(age15_64 = sum(count))

# join it up
jobseeker_table <- left_join(jobseeker_merge, vic_sa2_1564) %>% 
  mutate(pct_15_64 = round(total/age15_64*100, 1))

# export - to be filtered by month and data type then joined to shp
jobseeker_table_long <- jobseeker_table %>% 
  select(sa2, sa2_name, month, everything()) %>% 
  filter(age15_64 >= 250) %>% 
  pivot_longer(cols = -(sa2:month), names_to = "data_type", values_to = "values") %>% 
  mutate(data_type = case_when(data_type == "job_seeker_payment" ~ "JobSeeker payment recipients",
                               data_type == "youth_allowance_other" ~ "Youth Allowance recipients",
                               data_type == "total" ~ "Total JobSeeker and Youth allowance recipients",
                               data_type == "age15_64" ~ "Population aged 15-64",
                               data_type == "pct_15_64" ~ "Percentage aged 15-64 on either JobSeeker or Youth Allowance",
                               TRUE ~ NA_character_)) 
write_csv(jobseeker_table_long, "app_data/jobseeker_table_long.csv")

# join to age for app table part 2 of 2
mv_sa2_age <- asgs_age %>% 
  select(-(s_t_code:sa2_code)) %>% 
  rename(region = sa2_name) %>% 
  filter(region %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                       "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore"))

vic_age <- asgs_age %>% 
  select(-s_t_code, -(gccsa_code:sa2_name)) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(region = "Victoria")

gm_age <- asgs_age %>% 
  filter(gccsa_name == "Greater Melbourne") %>% 
  select(-(s_t_code:gccsa_code), -(sa4_code:sa2_name)) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(region = "Greater Melbourne")

# lga
lga_col_names <- c("S/T code", "S/T name", "LGA code", "LGA name", "Age0-4", "Age5–9", "Age10–14", "Age15–19", "Age20–24", "Age25–29", "Age30–34", "Age35–39", "Age40–44", "Age45–49", "Age50–54", "Age55–59", "Age60–64", "Age65–69", "Age70–74", "Age75–79", "Age80–84", "Age85 and over", "Total Persons")

lga_age <- read_excel("data_in/32350ds0003_lga_2018.xls", sheet = "Table 3", skip = 10, col_names = lga_col_names) %>% 
  clean_names() %>% 
  filter(s_t_name == "Victoria") %>% 
  select(-(s_t_code:lga_code)) %>% 
  rename(region = lga_name) %>% 
  filter(region == "Moonee Valley (C)") %>% 
  mutate(region = "City of Moonee Valley")

# join and get the 15-64 year olds
joined_ages <- bind_rows(mv_sa2_age, lga_age, vic_age, gm_age) %>% 
  select(region, age15_19:age60_64) %>% 
  pivot_longer(-region, names_to = "age_groups", values_to = "count") %>% 
  group_by(region) %>% 
  summarise(age15_64 = sum(count))

# join to the ages for the app table
jobseeker_joined <- left_join(jobseeker_all, joined_ages) %>% 
  mutate(pct_15_64 = round(total/age15_64*100, 1)) %>% 
  mutate(region = factor(region, levels = c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                                            "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore",
                                            "City of Moonee Valley", "Greater Melbourne", "Victoria"))) %>% 
  select(-job_seeker_payment, -youth_allowance_other, -age15_64) %>% 
  pivot_longer(cols = -(region:month), names_to = "data_type", values_to = "values") %>% 
  mutate(data_type = if_else(data_type == "total", "Total JobSeeker and Youth allowance recipients",
                             "Percentage aged 15-64 on either JobSeeker or Youth Allowance"))
write_csv(jobseeker_joined, "app_data/jobseeker_joined.csv")

js_data_list <- c("Total JobSeeker and Youth allowance recipients", "Percentage aged 15-64 on either JobSeeker or Youth Allowance", 
             "JobSeeker payment recipients", "Youth Allowance recipients")

js_month_list <- jobseeker_table_long %>% 
  distinct(month) %>% 
  arrange(desc(month)) %>% 
  pull()

js_month_list <- jobseeker_table_long %>% 
  distinct(month) %>% 
  arrange(desc(month)) %>% 
  pull()

jobseeker_month <- js_month_list[1]
jobseeker_first <- js_month_list[length(js_month_list)]


# tmap
tmap_mode("view")
tm_shape(js_map_join, bbox = tmaptools::bb(mv_shp)) +
  tm_fill("percentage") +
  tm_borders(alpha = 0.5, col = "grey") +
  tm_shape(mv_shp) +
  tm_borders(alpha = 0.5, col = "purple", lwd = 2)

