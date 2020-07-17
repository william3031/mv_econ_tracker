# jobkeeper data
# https://treasury.gov.au/coronavirus/jobkeeper/data
# postcode level data for organisations

# libraries
pacman::p_load(tidyverse, readxl, janitor, scales, sf, rmapshaper, tmap, tmaptools, leaflet)

# read in data
jk_raw <- read_excel("data_in/jobkeeper-data.xlsx", sheet = "Data") %>% 
  clean_names() %>% 
  rename(count = april_application_count) %>%  # change as needed
  filter(!is.na(count))
write_csv(jk_raw, "app_data/jk_raw.csv")

# join to selected postcodes and export # simplify the postcode file first!!!!!!!!!!!!!!!!!!!!!
postcodes_jk <- st_read("data_in/shp/postcodes_simplified.shp") 

jk_join <- left_join(postcodes_jk, jk_raw) %>%  
  filter(!is.na(count)) 


# perhaps just match to postcodes and have a table? #####################
jk_mv_postcodes <- jk_raw %>% 
  mutate(postcode = str_trim(postcode)) %>%
  filter(postcode != "TOTAL") %>% 
  filter(postcode %in% c("3031", "3032", "3033", "3034", "3039", "3040", "3041", "3042")) %>% 
  adorn_totals() %>% 
  mutate(count = comma(count)) %>% 
  rename(Postcode = postcode, Count = count)

## postcodes were simplified first using rmapshaper
#pex <- st_read("data_in/shp/postcodes_ex.shp") %>% 
#  clean_names() %>% 
#  ms_simplify() %>% 
#  select(postcode)
#st_write(pex, "data_in/shp/postcodes_simplified.shp")

# mv shapefile
mv_shp <- st_read("app_data/shp/mvcc_boundary.shp")

# tmap
tmap_mode("view")
tm_shape(jk_join, bbox = tmaptools::bb(mv_shp)) +
  tm_fill("count") +
  tm_borders(alpha = 0.5, col = "grey") +
  tm_shape(mv_shp) +
  tm_borders(alpha = 0.5, col = "purple", lwd = 2)

