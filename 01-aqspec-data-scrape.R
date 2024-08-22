rm(list=ls()) # Start with a clean environment

pacman::p_load(pacman, rvest, dplyr, magrittr, tibble) # Load packages with pacman

aqspec_html <- read_html("https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm")

colnames <- c("Make", "Est. Cost ($)", "Pollutant", "Field R2", "Field MAE (µg/m3)", "Lab MAE (µg/m3)")

pmsensor_table <- aqspec_html %>% 
  html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]] %>%
  select(Make = X2,Cost = X3,Pollutant = X4,FieldR2 = X5,FieldMAE = X8,LabMAE = X9) %>% 
  filter(!row_number() %in% c(1,2))
# TODO Separate FieldR2 into low and high https://tidyr.tidyverse.org/reference/separate.html

pmsensor_table
colnames

as.vector(pmsensor_table)
pmsensor_table %>% filter()

# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L