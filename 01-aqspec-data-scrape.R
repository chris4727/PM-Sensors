rm(list=ls()) # Start with a clean environment

pacman::p_load(pacman, rvest, dplyr) # Load packages with pacman

aqspec_url <- "https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm"
aqspec_html <- read_html(aqspec_url)

pmsensor_table <- aqspec_html %>% html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]]
pmsensor_table


# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L
