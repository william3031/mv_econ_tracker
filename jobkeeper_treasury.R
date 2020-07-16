# jobkeeper data
# https://treasury.gov.au/coronavirus/jobkeeper/data
# postcode level data for organisations

# libraries
pacman::p_load(tidyverse, readxl, janitor, scales, sf)

# read in data
jk_raw <- read_excel("data_in/jobkeeper-data.xlsx", sheet = "Data") %>% 
  clean_names() %>% 
  rename(count = april_application_count) %>%  # change as needed
  filter(!is.na(count))

# join to selected postcodes and export
postcodes_jk <- st_read("data_in/shp/postcodes_selected.shp") %>% 
  clean_names() %>% 
  left_join(jk_raw) %>% 
  filter(!is.na(count))
st_write(postcodes_jk, "app_data/shp/postcodes_selected.shp", delete_layer = TRUE)



# perhaps just match to postcodes and have a table? #####################
jk_raw %>% 
  mutate(postcode = str_trim(postcode)) %>%
  filter(postcode != "TOTAL") %>% 
  filter(postcode %in% c("3031", "3032", "3033", "3034", "3039", "3040", "3041", "3042"))


