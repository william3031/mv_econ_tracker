# https://www.abs.gov.au/ausstats/abs@.nsf/mf/6160.0.55.001
# ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia

# libraries
pacman::p_load(tidyverse, readxl, ggrepel, janitor, scales, glue, lubridate)

# dates for glue
latest_week <- "27 June"
abs_publication_date <- "15 July 2020"

# read in raw data - VICTORIAN DATA
weekly_jobs_data_raw <- read_excel("data_in/6160055001_do004_150720.xlsx", sheet = "Payroll jobs index", skip = 5) %>% 
  clean_names() %>% 
  filter(state_or_territory == "2. VIC") %>% 
  mutate(type = "jobs")

weekly_wages_data_raw <- read_excel("data_in/6160055001_do004_150720.xlsx", sheet = "Total wages index", skip = 5) %>% 
  clean_names() %>% 
  filter(state_or_territory == "2. VIC") %>% 
  mutate(type = "wages")

#filter and join
weekly_jobs_data <- weekly_jobs_data_raw %>% 
  select(industry_division, sex, age_group, last = tail(names(.), 2)) %>%  # the last two columns
  rename(latest_week = last1, type = last2)

weekly_wages_data <- weekly_wages_data_raw %>% 
  select(industry_division, sex, age_group, last = tail(names(.), 2)) %>%  # the last two columns
  rename(latest_week = last1, type = last2)

weekly_abs_data <- bind_rows(weekly_jobs_data, weekly_wages_data) %>% 
  mutate(sex = str_remove(sex, ".*? ")) %>% 
  mutate(age_group = str_remove(age_group, ".*? ")) %>% 
  mutate(industry_division = str_remove(industry_division, ".*? ")) %>% 
  mutate(age_group = factor(age_group, levels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70 and over", "All ages"))) %>% 
  mutate(industry_division = if_else(industry_division == "All industries", "All industries", str_sub(industry_division, 3, -1))) %>% 
  mutate(industry_division= factor(industry_division)) %>% 
  mutate(latest_week = as.numeric(latest_week)) %>% 
  mutate(sex = factor(sex))

# jobs wages index data
jobs_wages_index <- bind_rows(weekly_jobs_data_raw, weekly_wages_data_raw) %>% 
  filter(industry_division == "0. All industries", sex == "0. Persons", age_group == "0. All ages") %>% 
  select(-industry_division, -sex, -age_group) %>% 
  pivot_longer(cols = -type, names_to = "date", values_to = "values") %>% 
  mutate(date = str_remove(date, "x")) %>% 
  mutate(date = excel_numeric_to_date(as.numeric(as.character(date)),
                                      date_system = "modern")) %>% 
  mutate(values = as.numeric(values)) %>% 
  mutate(values = round(values, 1)) %>% 
  filter(date >= "2020-03-14") %>% 
  mutate(values = round(values - 100, 1))
write_csv(jobs_wages_index, "app_data/jobs_wages_index.csv")

# industry division list - use this as a filter for the bottom graph in the app
ind_div_list <- weekly_abs_data %>% 
  distinct(industry_division) %>% 
  pull()

# data for age and gender - victoria
jobs_wages_by_age_data <- weekly_abs_data %>% 
  filter(sex %in% c("Males", "Females")) %>% 
  filter(industry_division == "All industries") %>% 
  mutate(latest_week = round(latest_week - 100, 1))
write_csv(jobs_wages_by_age_data, "app_data/jobs_wages_by_age_data.csv")

weekly_abs_data %>% 
  distinct(industry_division)
