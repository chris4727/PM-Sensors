rm(list=ls()) # Start with a clean environment
#install.packages("tidyverse")
pacman::p_load(pacman, rvest, tidyr, dplyr, magrittr, stringr, visdat, gtExtras) # Load packages with pacman

aqspec_html <- read_html("https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm")

pmsensor_table <- aqspec_html %>% 
  html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]] %>%
  select(Make = X2,Cost = X3,Pollutant = X4,FieldR2 = X5,FieldMAE = X8,LabMAE = X9) %>% 
  filter(!row_number() %in% c(1,2) & Pollutant == "PM2.5") %>%
  select(!Pollutant) %>%
  separate_wider_delim(col = FieldR2, delim = " to ", names = c("FieldR2lo", "FieldR2hi"), too_few = "align_start") %>%
  separate_wider_delim(col = FieldMAE, delim = " to ", names = c("FieldMAElo", "FieldMAEhi"), too_few = "align_start") %>%
  separate_wider_delim(col = LabMAE, delim = " to ", names = c("LabMAElo", "LabMAEhi"), too_few = "align_start") %>%
  mutate(FieldMAElo = na_if(FieldMAElo, "")) %>%
  mutate(LabMAElo = na_if(LabMAElo, ""))
  
# Create a table to view the data
pmsensor_table %>%
  filter(FieldR2lo >= 0.7) %>%
  arrange(desc(FieldR2lo),Cost) %>%
  select(Make, Cost, FieldR2lo, FieldR2hi) %>%
  gt() %>%
  tab_header(md("$PM_{2.5}$ Sensors with a Field $R^{2}$ of at least 0.7")) %>% 
  gt_theme_nytimes()

  # TODO change "~0.0" value to "0.0" in FieldR2lo
  #   str_replace("~", "")
  # TODO change empty items to NA
  # TODO Assign the correct data types to all variables

vis_dat(pmsensor_table) # Visualize datatypes
vis_miss(pmsensor_table) # Visualize missing values


# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L
