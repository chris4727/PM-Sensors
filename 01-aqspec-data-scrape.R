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

## Explore data
#glimpse(pmsensor_table)
#View(pmsensor_table)
#names(pmsensor_table)
#pmsensor_table
#
## Visualise data with "visdat" package
#vis_dat(pmsensor_table) # Visualize datatypes
#vis_miss(pmsensor_table) # Visualize missing values

#------------------------------------------------------------------------
# 2026-02-03 Git all code working with new AQ-SPEC table up to this point
# [ ] Move table creation and data visualization to another script

# Create a table to view the data
pmsensor_table %>%
  filter(fieldr2lo >= 0.7) %>%
  arrange(desc(fieldr2lo),cost) %>%
  select(make, cost, fieldr2lo, fieldr2hi) %>%
  gt() %>%
  tab_header(
    title = html("AQ-SPEC PM<sub>2.5</sub> Sensors"),
    subtitle = md("Sensors with a field R^2^ of at least 0.7 compared to a reference monitor")
    ) %>%
  # TODO The md() function renders "PM~2.5~" as strikethrough. Bug? Using html as workaround.
  tab_source_note(md("**Source:** [South Coast AQMD’s AQ-SPEC program PM Sensor Evaluations](https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm)")) %>% 
  # TODO Programmatically insert the link the data was pulled from
  # TODO Programmatically include the date accessed
  tab_spanner(
    label = "Field {{R^2}}",
    columns = fieldr2lo:fieldr2hi
    ) %>%
  tab_footnote(
    footnote = md("The coefficient of determination (R^2^) is a statistical parameter measuring the degree of relation between two variables. Here, it measures the linear relationship between the sensor and the Federal Reference Method (FRM), or Federal Equivalent Method (FEM), or Best Available Technology (BAT) reference instrument. An R^2^ approaching the value of 1 reflects a near perfect correlation, whereas a value of 0 indicates a complete lack of correlation. All R^2^ values reported in these reports are based either on 5-min or 1-hr average data."),
    locations = cells_column_spanners(spanners = everything())
    ) %>% 
  cols_label(
    make ~ "{{PM_2.5}} Sensor Model",
    cost ~ "Cost",
    fieldr2lo ~ "Low",
    fieldr2hi ~ "High"
    ) %>%
  fmt_currency(
    columns = cost,
    currency = "USD"
    ) %>% 
  gt_theme_nytimes()
  # TODO Apply viridis color palette field to the cost column


# Create a plot of Cost vs FieldR2lo
pmsensor_table %>%
  ggplot(aes(cost,fieldr2lo)) +
  geom_point(aes(colour = fieldmaelo)) +
  #geom_smooth()+
  # Method = Linear Model, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = expression("Field R"^2~" vs Cost of PM"[2.5]*" Sensors"),
       x = "Cost (USD)",
       y = expression("Field R"[2])
       )+
  theme_minimal()

# Create a plot of Cost vs FieldMAElo
pmsensor_table %>%
  ggplot(aes(cost,fieldmaelo)) +
  geom_point(aes(colour = fieldr2lo)) +
  #geom_smooth()+
  # Method = Linear Model, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = expression("Field MAE vs Cost of PM"[2.5]*" Sensors"),
       x = "Cost (USD)",
       y = "Field Mean Analytical Error (MAE)"
       )+
  theme_minimal()

# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L
