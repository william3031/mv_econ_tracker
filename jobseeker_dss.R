# jobseeker and youth allowance data
# https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile

# libraries
pacman::p_load(tidyverse, readxl, janitor, scales, sf)

# don't use purrr on this as the files are the same, cant' work out month
data_mar20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-march-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-03-01")

data_apr20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-april-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-04-01")

data_may20 <- read_excel("data_in/jobseeker-payment-and-youth-allowance-recipients-monthly-profile-may-2020.xlsx",
                         sheet = "Table 4 - By SA2", skip = 6) %>%
  clean_names() %>% remove_empty() %>%
  mutate(month = "2020-05-01")

### merge it all
jobseeker_merge <- bind_rows(data_mar20, data_apr20, data_may20) %>% 
  mutate(job_seeker_payment = parse_number(job_seeker_payment), youth_allowance_other = parse_number(youth_allowance_other)) %>% 
  mutate(month = ymd(month)) %>%
  mutate(total = job_seeker_payment + youth_allowance_other)

jobseeker_merge_sa2 <- jobseeker_merge %>% 
  filter(sa2_name %in% c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                         "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore")) %>% 
  select(-sa2) %>% 
  rename(region = sa2_name)

jobseeker_merge_mv <- jobseeker_merge_sa2 %>% 
  group_by(month) %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "City of Moonee Valley")

jobseeker_merge_gm <- jobseeker_merge %>% 
  filter((sa2 >= 21105 & sa2 <= 21385)| sa2 >= 21424 & sa2 <= 21468) %>% 
  group_by(month)  %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "Greater Melbourne")

jobseeker_merge_vic <- jobseeker_merge %>% 
  filter((sa2 >= 20000 & sa2 <= 29999)) %>% 
  group_by(month)  %>% 
  summarise(job_seeker_payment = sum(job_seeker_payment), youth_allowance_other = sum(youth_allowance_other), total = sum(total)) %>% 
  mutate(region = "Victoria")

# everything joined
jobseeker_all <- bind_rows(jobseeker_merge_sa2, jobseeker_merge_mv, jobseeker_merge_gm, jobseeker_merge_vic) %>% 
  rename(total_js_ya = total)

# add 15-64 population proportion ########################################################################

# asgs
asgs_col_names <- c("S/T code", "S/T name", "GCCSA code", "GCCSA name", "SA4 code", "SA4 name", "SA3 code", "SA3 name", "SA2 code", "SA2 name", "Age0-4", "Age5–9", "Age10–14", "Age15–19", "Age20–24", "Age25–29", "Age30–34", "Age35–39", "Age40–44", "Age45–49", "Age50–54", "Age55–59", "Age60–64", "Age65–69", "Age70–74", "Age75–79", "Age80–84", "Age85 and over", "Total Persons")

asgs_age <- read_excel("data_in/32350ds0001_asgs2016_2018.xls", sheet = "Table 3", skip = 9, col_names = asgs_col_names) %>% 
  clean_names() %>% 
  filter(s_t_name == "Victoria") 

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

# join to the ages
jobseeker_joined <- left_join(jobseeker_all, joined_ages) %>% 
  mutate(pct_15_64 = round(total_js_ya/age15_64*100, 1)) %>% 
  mutate(region = factor(region, levels = c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                                            "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore",
                                            "City of Moonee Valley", "Greater Melbourne", "Victoria")))

jobseeker_joined_mv_sa2 <- jobseeker_joined %>% 
  filter(!region %in% c("City of Moonee Valley", "Greater Melbourne", "Victoria"))


# spatial data - simplify the file perhaps
sa2_greater <- st_read("data_in/shp/sa2_2016_gmel.shp") %>% 
  clean_names() 

# perhaps calculate the lastest month and show the counts and rates for all?

