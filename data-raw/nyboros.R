#' county fips codes for 5 boros of NYC

library(tidyverse)
rm(list = ls())


nyboros <- c("085", "005", "047", "061", "081")


usethis::use_data(nyboros, overwrite = TRUE)
