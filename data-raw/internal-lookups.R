library(usethis)
library(dplyr)
library(fwabc)

lookup_gnis <- fwa_lookup_stream %>%
  select(WATERSHED_KEY, GNIS_NAME) %>%
  distinct() %>%
  filter(!is.na(GNIS_NAME))

lookup_coast <- merge(fwa_lookup_coastline, fwa_lookup_watershedgroup, by = "WATERSHED_GROUP_CODE")
lookup_coast_wsgroup <- lookup_coast %>%
  select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME) %>%
  distinct()

select_key <- function(x) x %>% select(WATERSHED_KEY, FWA_WATERSHED_CODE)
lookup_wskey <- bind_rows(
  select_key(fwa_lookup_stream),
  select_key(fwa_lookup_watershed),
  select_key(fwa_lookup_coastline)) %>%
  distinct()

use_data(lookup_gnis, lookup_wskey, lookup_coast_wsgroup, internal = TRUE, overwrite = TRUE)
