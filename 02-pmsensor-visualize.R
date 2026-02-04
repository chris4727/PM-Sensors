rm(list=ls()) # Start with a clean environment
## Force precompiled V8 install to avoid error in gtExtras dependancy installation
#Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
#install.packages("V8")
#install.packages("gtExtras")
library(tidyverse)
library(gtExtras)
theme_set(theme_minimal())

updated <- "2026-02-03"
glue("./data/pmsensors_{Sys.Date()}.csv")
pmsensor_table <- read_csv(glue("./data/pmsensors_{updated}.csv"))

# Explore data
glimpse(pmsensor_table)
View(pmsensor_table)
names(pmsensor_table)
pmsensor_table
gt_plt_summary(pmsensor_table)

# Visualise data with "visdat" package
vis_dat(pmsensor_table) # Visualize datatypes
vis_miss(pmsensor_table) # Visualize missing values



#------------------------------------------------------------------------
# 2026-02-03 Git all code working with new AQ-SPEC table up to this point

# Select only sensors with field R2 >= 0.7
pmsensors <- pmsensor_table %>%
  filter(fieldr2lo >= 0.7)

# Create a table to view the data
pmsensor_table1 <- pmsensors %>% 
  arrange(desc(fieldr2lo),cost) %>%
  select(model, cost, fieldr2lo, fieldr2hi, fieldmaelo) %>%
  gt(id = "pmsensors") %>% 
  # Add title
  tab_header(
    title = html("AQ-SPEC PM<sub>2.5</sub> Sensors"),
    subtitle = md("Sensors with a field R^2^ of at least 0.7 compared to a reference monitor")
    ) %>% 
  # TODO The md() function renders "PM~2.5~" as strikethrough. Bug? Using html as workaround.
  tab_source_note(md(glue("**Source:** [South Coast AQMD’s AQ-SPEC program PM Sensor Evaluations](https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm) Accessed {updated}"))) %>% 
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
  tab_footnote(
    footnote = md("The field Mean Absolute Error (MAE) is is the absolute difference between the sensors and the reference instruments. The larger MAE values, the higher measurement errors as compared to the reference instruments."),
    locations = cells_column_labels(columns = fieldmaelo)
    ) %>% 
  cols_label(
    model ~ "{{PM_2.5}} Sensor Model",
    cost ~ "Cost (USD)",
    fieldr2lo ~ "Low",
    fieldr2hi ~ "High",
    fieldmaelo ~ "Field MAE"
    ) %>%
  fmt_currency(
    columns = cost,
    currency = "USD"
    ) %>% 
  # TODO Apply viridis color palette field to the cost column
  gt_theme_538()


# Create a plot of Cost vs FieldR2lo
pmsensor_table %>%
  ggplot(aes(cost,fieldr2lo))
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
