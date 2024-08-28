rm(list=ls()) # Start with a clean environment
#install.packages("tidyverse")
pacman::p_load(pacman, rvest, tidyr, dplyr, magrittr, stringr, visdat, gtExtras, ggplot2, viridis) # Load packages with pacman

# Pull AQ-Spec website data
aqspec_html <- read_html("https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm")

# Create dataframe from website html and format data
pmsensor_table <- aqspec_html %>% 
  html_elements(".telerik-reTable-1") %>%
  html_table() %>% .[[1]] %>%
  select(make = X2,cost = X3,pollutant = X4,fieldr2 = X5,fieldmae = X8,labmae = X9) %>% 
  filter(!row_number() %in% c(1,2) & pollutant == "PM2.5") %>%
  select(!pollutant) %>%
  separate_wider_delim(col = fieldr2, delim = " to ", names = c("fieldr2lo", "fieldr2hi"), too_few = "align_start") %>%
  separate_wider_delim(col = fieldmae, delim = " to ", names = c("fieldmaelo", "fieldmaehi"), too_few = "align_start") %>%
  separate_wider_delim(col = labmae, delim = " to ", names = c("labmaelo", "labmaehi"), too_few = "align_start") %>%
  mutate(
    cost = as.numeric(gsub("[~\\$,]", "", pmsensor_table$cost)),
    fieldr2lo = as.numeric(pmsensor_table$fieldr2lo),
    fieldr2hi = as.numeric(pmsensor_table$fieldr2hi),
    fieldmaelo = as.numeric(na_if(fieldmaelo, "")),
    fieldmaehi = as.numeric(pmsensor_table$fieldmaehi),
    labmaelo = as.numeric(na_if(labmaelo, "")),
    labmaehi = as.numeric(pmsensor_table$labmaehi),
    )

pmsensor_table

# Visualise data
vis_dat(pmsensor_table) # Visualize datatypes
vis_miss(pmsensor_table) # Visualize missing values
  
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
  geom_point() +
  #geom_smooth()+
  # Method = Linear Model, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = "Field R2 vs Cost of PM2.5 Sensors",
       x = "Cost (USD)",
       y = "Field R2"
       )+
  theme_minimal()

# Cleanup the environment
dev.off()   # Clear plots if there is one
cat("\014") # Clear console. Same as Ctrl+L
