library(usethis)
library(dplyr)
library(fwabc)

lookup_stream_gnis <- fwa_lookup_stream[!is.na(fwa_lookup_stream$GNIS_NAME),]
lookup_wshed_gnis <- fwa_lookup_watershed[!is.na(fwa_lookup_watershed$GNIS_NAME),]
lookup_coast <- merge(fwa_lookup_coastline, fwa_lookup_watershedgroup, by = "WATERSHED_GROUP_CODE")
lookup_coast_wsgroup <- lookup_coast %>%
  select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME) %>%
  distinct()

use_data(lookup_stream_gnis, lookup_wshed_gnis, lookup_coast_wsgroup, internal = TRUE, overwrite = TRUE)
