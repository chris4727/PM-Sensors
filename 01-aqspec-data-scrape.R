rm(list=ls()) # Start with a clean environment

pacman::p_load(pacman, rvest, dplyr, magrittr, tibble) # Load packages with pacman

aqspec_html <- read_html("https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm")

pmsensor_table <- aqspec_html %>% 
  html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]] %>%
  select(Make = X2,Cost = X3,Pollutant = X4,FieldR2 = X5,FieldMAE = X8,LabMAE = X9) %>% 
  filter(!row_number() %in% c(1,2) & Pollutant == "PM2.5") %>%
  select(!Pollutant) %>%
  separate_wider_delim(col = FieldR2, delim = " to ", names = c("FieldR2lo", "FieldR2hi"), too_few = "align_start")
  # TODO change "~0.0" value to "0.0" in FieldR2lo
  # TODO Separate FieldMAE and LabMAE into lo and hi https://tidyr.tidyverse.org/reference/separate.html
  # TODO convert FieldR2, FieldMAE, and LabMAE lo and hi into numbers

pmsensor_table

# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L