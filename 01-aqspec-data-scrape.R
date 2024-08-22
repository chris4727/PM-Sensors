rm(list=ls()) # Start with a clean environment

pacman::p_load(pacman, rvest, dplyr, magrittr, tibble) # Load packages with pacman

aqspec_url <- "https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm"
aqspec_html <- read_html(aqspec_url)

colnames <- c("Make", "Est. Cost ($)", "Pollutant", "Field R2", "Field MAE (µg/m3)", "Lab MAE (µg/m3)")

pmsensor_table <- aqspec_html %>% 
  html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]] %>%
  select(c(X2,X3,X4,X5,X8,X9)) %>% 
  filter(!row_number() %in% 1) # TODO Remove current columnheaders, replace with row 1 or colnames

pmsensor_table
colnames

as.vector(pmsensor_table)
pmsensor_table %>% filter()

# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L