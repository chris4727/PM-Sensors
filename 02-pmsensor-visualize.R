rm(list=ls()) # Start with a clean environment
## Force precompiled V8 install to avoid error in gtExtras dependancy installation
#Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
#install.packages("V8")
#install.packages("gtExtras")
library(tidyverse)
library(glue)
library(gtExtras)
theme_set(theme_minimal())

updated <- "2026-02-03"
glue("./data/pmsensors_{Sys.Date()}.csv")
pmsensors <- read_csv(glue("./data/pmsensors_{updated}.csv"))

# Indicate sensors with good correlation
pmsensors <- pmsensors %>%
  mutate(
    # Create new column indicating if R2 is greater than 0.7
    # TODO good == fieldr2lo > 0.7
    # TODO med == fieldr2lo <= 0.7, fieldr2hi > 0.7
    # TODO low == fieldr2hi <= 0.7
    correlation = if_else(fieldr2lo > 0.7, "good", "low")
    )

# Explore data
glimpse(pmsensors)
View(pmsensors)
names(pmsensors)
pmsensors
gt_plt_summary(pmsensors)

# Visualise data with "visdat" package
vis_dat(pmsensors) # Visualize datatypes
vis_miss(pmsensors) # Visualize missing values


# Create a table to view the data
pmsensors_table1 <- pmsensors %>% 
  arrange(fieldmaelo,cost) %>%
  filter(correlation == "good") %>% 
  select(model, cost, fieldr2lo, fieldr2hi, fieldmaelo, fieldmaehi) %>%
  gt(id = "pmsensors") %>% 
  # Add title
  tab_header(
    title = html("AQ-SPEC PM<sub>2.5</sub> Sensors"),
    subtitle = md("Sensors with a field R^2^ of at least 0.7 compared to a reference monitor arranged by Field MAE")
    ) %>% 
  # TODO The md() function renders "PM~2.5~" as strikethrough. Bug? Using html as workaround.
  tab_source_note(md(glue("**Source:** [South Coast AQMD’s AQ-SPEC program PM Sensor Evaluations](https://www.aqmd.gov/aq-spec/evaluations/criteria-pollutants/summary-pm) Accessed {updated}"))) %>% 
  # TODO Programmatically insert the link the data was pulled from
  # TODO Programmatically include the date accessed
  tab_spanner(
    label = "Field {{R^2}}",
    columns = fieldr2lo:fieldr2hi
    ) %>% 
  tab_spanner(
    label = "Field MAE",
    columns = fieldmaelo:fieldmaehi
    ) %>% 
  tab_footnote(
    footnote = md("The coefficient of determination (R^2^) is a statistical parameter measuring the degree of relation between two variables. Here, it measures the linear relationship between the sensor and the Federal Reference Method (FRM), or Federal Equivalent Method (FEM), or Best Available Technology (BAT) reference instrument. An R^2^ approaching the value of 1 reflects a near perfect correlation, whereas a value of 0 indicates a complete lack of correlation. All R^2^ values reported in these reports are based either on 5-min or 1-hr average data."),
    locations = cells_column_labels(columns = fieldr2lo)
    ) %>% 
  tab_footnote(
    footnote = md("The field Mean Absolute Error (MAE) is is the absolute difference between the sensors and the reference instruments. The larger MAE values, the higher measurement errors as compared to the reference instruments."),
    locations = cells_column_labels(columns = fieldmaelo)
    ) %>% 
  cols_label(
    model ~ "{{PM_2.5}} Sensor model",
    cost ~ "Cost (USD)",
    fieldr2lo ~ "Low",
    fieldr2hi ~ "High",
    fieldmaelo ~ "Low",
    fieldmaehi ~ "High"
    ) %>%
  fmt_currency(
    columns = cost,
    currency = "USD"
    ) %>% 
  # TODO Apply viridis color palette field to the cost column
  gt_theme_538()
pmsensors_table1
# Save the resulting table
pmsensors_table1 %>%
  gtsave(filename = "./output/pmsensors_table1.html")
#pmsensors1 %>%
#  gtsave(filename = "./output/pmsensors1.png", expand = 10)


# Test hypothesis that less expensive PM2.5 sensors are less accurate.
#   In other words, that higher cost results in higher field R2.
#   Null hypothesis: Correlation is less than or equal to 0

# Correlation test:
cor.test(pmsensors$fieldr2lo,
         pmsensors$cost)
# Linear regression
#   Linear fig1lm: Y(fieldR2) is explained by cost
fig1lm <- lm(fieldr2lo ~ cost,
   data = pmsensors)
summary(fig1lm)
# Save the R2 of the linear model later used in Figure 1
fig1r2 <- round(summary(fig1lm)$r.squared, digits = 3)

# Figure 1 Plot of Cost vs FieldR2lo
# -----------------------------------------------------------
fig1 <- pmsensors %>%
  filter(fieldr2lo > 0.7) %>% 
  ggplot(aes(x = cost,
             y = fieldr2lo)) +
            # Color by FieldR2 above or below 0.7
  geom_point(size = 4,
             alpha = 0.5) +
  # Method = Linear fig1lm, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = expression("Field R"^2~" vs Cost of PM"[2.5]*" Sensors"),
       # Set axis labels to display with "$" for USD
       x = "Cost (USD)",
       y = expression("Field R"[2])
       ) +
  annotate(geom = "text",
           x = 6000,
           y = 0.9,
           label = glue("R2 {fig1r2}")
  )
ggsave("fig1costVr2.png",
       height = 4,
       width = 6
)
fig1

# Figure 2 Lognormal plot of Cost vs FieldR2lo
# -----------------------------------------------------------
#Hist of cost
pmsensors %>%
  ggplot(aes(x = cost)) +
           geom_histogram(binwidth = 300)
#Hist of the log of cost
pmsensors %>%
  ggplot(aes(x = log(cost))) +
           geom_histogram()

# Linear regression
#   Linear fig1lm: Y(fieldR2) is explained by cost
fig2lm <- lm(fieldr2lo ~ log(cost),
   data = pmsensors)
summary(fig2lm)
# Save the R2 of the linear model later used in Figure 1
fig2r2 <- round(summary(fig2lm)$r.squared, digits = 3)

fig2log <- pmsensors %>% 
  filter(fieldr2lo > 0.7) %>% 
  ggplot(aes(x = log(cost),
             y = fieldr2lo)) +
            # Color by FieldR2 above or below 0.7
  geom_point(size = 4,
             alpha = 0.5) +
  # Method = Linear fig1lm, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = expression("Field R"^2~" vs Cost of PM"[2.5]*" Sensors"),
       # Set axis labels to display with "$" for USD
       x = "Cost (USD)",
       y = expression("Field R"[2])
       ) +
  annotate(geom = "text",
           x = 6,
           y = 0.9,
           label = glue("R2 {fig2r2}")
  )
fig2log
ggsave("fig2logcostVr2.png",
       height = 4,
       width = 6
)

# Figure 3 Plot of Cost vs FieldR2lo including low R2 sensors
# -----------------------------------------------------------
fig3 <- pmsensors %>%
  #ggplot(aes(x = cost,
  #           y = fieldr2lo,
  #           color = correlation)) +
  ggplot(aes(x = cost,
             y = fieldr2lo,
             color = correlation)) +
            # Color by FieldR2 above or below 0.7
  geom_point(size = 4,
             alpha = 0.5) +
  # Method = Linear fig1lm, Standard Error = False
  # TODO Do linear regression to get the R2 of this comparison
  geom_smooth(
    method = lm, 
    se = F) +
  labs(title = expression("Field R"^2~" vs Cost of PM"[2.5]*" Sensors"),
       # Set axis labels to display with "$" for USD
       x = "Cost (USD)",
       y = expression("Field R"[2])
       )
fig3
         


# Create a plot of Cost vs FieldMAElo
pmsensors %>%
  ggplot(aes(cost,fieldmaelo)) +
  geom_point(aes(colour = fieldr2lo)) +
  #geom_smooth()+
  # Method = Linear fig1lm, Standard Error = False
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
