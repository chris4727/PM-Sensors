rm(list=ls()) # Start with a clean environment
#install.packages("tidyverse")
library(tidyverse)
library(rvest)
library(visdat)
library(glue)
#pacman::p_load(pacman, rvest, tidyr, dplyr, magrittr, stringr, visdat, gtExtras, ggplot2, viridis) # Load packages with pacman

# Pull AQ-Spec website data
aqspec_html <- read_html("https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm")

# Create dataframe from website html and format data
pmsensor_table <- aqspec_html %>% 
  # Select the HTML element name of the table.
  # Find the element with https://rvest.tidyverse.org/articles/selectorgadget.html
  html_elements(".backdrop") %>%
  # Convert HTML table to dataframe
  html_table() %>%  .[[1]] %>%
  # Select desired columns and rename for clarity
  select(model = "Make (Model)",
         cost = "Est. Cost\n            (USD)",
         pollutant = "Pollutant \n            Tested\n            (Sensor Type)",
         fieldr2 = "Field R2",
         fieldmae = "Field MAE\n            (PM: µg/m3\n            Gas: ppb)") %>% 
  # Keep only rows from PM2.5 sensors
  filter(pollutant == "PM2.5") %>% 
  # Split fieldR2 and field MAE into separate low and high columns:
  separate_wider_delim(col = fieldr2, delim = " to ", names = c("fieldr2lo", "fieldr2hi"), too_few = "align_start") %>%
  separate_wider_delim(col = fieldmae, delim = " to ", names = c("fieldmaelo", "fieldmaehi"), too_few = "align_start") %>% 
  mutate(
    # Remove newline and extra spaces from model names
    model = str_replace_all(model, "\\s*\\n\\s*", " "),
    cost = as.numeric(cost),
    pollutant = as.factor(pollutant),
    fieldr2lo = as.numeric(fieldr2lo),
    fieldr2hi = as.numeric(fieldr2hi),
    # Set the "-" field MAE to NA
    fieldmaelo = as.numeric(na_if(fieldmaelo, "-")),
    fieldmaehi = as.numeric(fieldmaehi)
    )

# Write pmsensor_table to .csv with current date
filename <- glue("./data/pmsensors_{Sys.Date()}.csv")
write_csv(pmsensor_table, filename)
