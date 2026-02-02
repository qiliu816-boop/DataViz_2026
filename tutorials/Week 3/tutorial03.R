###############################
# Data Viz  
# Tutorial 3:  
# Relationships & Comparisons        
###############################  

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
    package.list <- setdiff(package.list, basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
    }
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg,  dependencies = TRUE)
    sapply(pkg,  require,  character.only = TRUE)
    }

# Load any necessary packages
lapply(c("tidyverse", "ggplot2", "lubridate", "patchwork", "geofacet", "ggrepel", "GGally", "corrgram", "ggcorrplot", "broom", "marginaleffects"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# Import dataset and load library
DUB_weather <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/daily_weather_DUB.csv")
CORK_weather <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/daily_weather_CORK.csv")
# we'll only look at 2025
DUB_weather <- DUB_weather[30317:nrow(DUB_weather),]
CORK_weather <- CORK_weather[23012:nrow(CORK_weather),]
IRE_weather <- rbind(subset(DUB_weather, select=-c(g_rad)), CORK_weather)
# need to alter date variable to not be character
IRE_weather$date <- dmy(DUB_weather$date)

# create dual axis plot with C and F on opposing y-axes
pdf("DUB_temp.pdf")
ggplot(IRE_weather[IRE_weather$station=="Dublin",], aes(x = date, y = maxtp)) +
  geom_line() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ (. * 9/5) + 32, name = "Fahrenheit")) +
  labs(x = "Date", y = "Celsius") +
  theme_minimal()
dev.off()

temp_plot <- ggplot(IRE_weather[IRE_weather$station=="Dublin",], aes(x = date, y = maxtp)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ (. * 9/5) + 32, name = "Fahrenheit")) +
  labs(x = "Date", y = "Celsius") +
  theme_minimal()

sun_plot <- ggplot(IRE_weather[IRE_weather$station=="Dublin",], aes(x = date, y = sun)) +
  geom_line() +
  geom_smooth() +
  labs(x = NULL, y = "Sunshine Duration") +
  theme_minimal()

pdf("patch_weather_plot.pdf")
# library(patchwork)
temp_plot + sun_plot
dev.off()

# load EU gdp time-series data
eu_gdp <- eu_gdp

pdf("geo_facet.pdf")
# library("geofacet")
ggplot(eu_gdp, aes(year, gdp_pc)) +
  geom_line() +
  facet_geo(~ name, grid = "eu_grid1", scales = "free_y") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  ylab("GDP Per Capita in Relation to EU Index (100)") +
  theme_bw()
dev.off()

# create "Northern Europe" indicator based on UN geoscheme
eu_gdp$north_europe <- ifelse(eu_gdp$name %in% c("Denmark", "Estonia", "Finland", "Ireland", "Latvia", "Lithuania", "Norway", "Sweden", "United Kingdom"), 1, 0)

pdf("north_europe_gdp.pdf")
ggplot(data = eu_gdp |> filter(north_europe == 1), aes(x = year, y = gdp_pc)) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(name), scales = "free_y", nrow = 3) +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))
dev.off()

pdf("north_europe_slope.pdf")
north_europe_gdp <- eu_gdp |> filter(north_europe == 1) |> filter(year %in% c(2004, 2015)) |> 
  mutate(label_first = ifelse(year == 2004, paste0(name, ": ", label_dollar(prefix = "€")(round(gdp_pc))), NA),
         label_last = ifelse(year == 2015, label_dollar(prefix = "€")(round(gdp_pc, 0)), NA))
ggplot(north_europe_gdp, aes(x = year, y = gdp_pc, group = name, color = name)) +
  geom_line(linewidth = 1.5) +
  labs(y="GDP Per Capita", x="Year", color="Country")
dev.off()

pdf("north_europe_slope_labs.pdf")
# library(ggrepel)
ggplot(north_europe_gdp, aes(x = year, y = gdp_pc, group = name, color = name)) +
  geom_line(linewidth = 1.5) +
  geom_text_repel(aes(label = label_first), direction = "y", nudge_x = -1, seed = 1234) +
  geom_text_repel(aes(label = label_last), direction = "y", nudge_x = 1, seed = 1234) +
  guides(color = "none") +
  theme_void()
dev.off()

pdf("weather_corr_pairs.pdf")
# library(GGally)
weather_cor <- IRE_weather |> select(rain, maxtp, mintp, wdsp, evap)
ggpairs(weather_cor)
dev.off()

pdf("weather_corr_pairs1.pdf")
# library(corrgram)
corrgram(weather_cor)
dev.off()
pdf("weather_corr_pairs2.pdf")
# library(ggcorrplot)
ggcorrplot(cor(weather_cor))
dev.off()

# run our "simple" regression w/ max temp as outcome
model_simple <- lm(maxtp ~ rain, data = IRE_weather)
# library(broom)
tidy(model_simple, conf.int = TRUE)

pdf("simple_reg.pdf")
ggplot(IRE_weather,
       aes(x = rain, y = maxtp)) +
  geom_point() +
  geom_smooth(method = "lm")
dev.off()

# run our "complex" regression w/ max temp as outcome
model_complex <- lm(maxtp ~ rain + mintp + wdsp + evap + station,
                    data = IRE_weather)
tidy(model_complex, conf.int = TRUE)

# coef plot
pdf("coef_plot.pdf")
ggplot(tidy(model_complex, conf.int = TRUE) |> filter(term != "(Intercept)"),
       aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) + 
  labs(x = "Coefficient estimate", y = NULL) +
  theme_minimal()
dev.off()

# library(marginaleffects)
# Calculate predictions across a range of windSpeed
predicted_values_easy <- predictions(model_complex,
  newdata = datagrid(wdsp = seq(3, 20, 0.5))
)

pdf("marginal_effect_wind.pdf")
ggplot(predicted_values_easy, aes(x = wdsp, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = "#BF3984", alpha = 0.5) + 
  geom_line(linewidth = 1, color = "#BF3984") +
  labs(x = "Mean Wind Speed (knot)", y = "Predicted high temperature (C)") +
  theme_minimal()
dev.off()

model_wild <- lm(maxtp ~ rain + mintp + wdsp + evap + station + I(wdsp^2) + wdsp:station, data = IRE_weather)

predicted_values_wild <- predictions(
  model_wild,
  newdata = datagrid(
    wdsp = seq(3, 20, 0.5),
    station = c("Cork", "Dublin")))

pdf("marginal_effect_wind_int.pdf")
ggplot(predicted_values_wild, aes(x = wdsp, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = station)) +
  geom_line(aes(color = station),linewidth = 1) +
  labs(x = "Mean Wind Speed (knot)", y = "Predicted high temperature (C)") +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  facet_wrap(vars(station), nrow = 1)
dev.off()

##############
### Group work
##############

# We'll be analyzing "The Judicial Review of Congress dataset", 
# which catalogs all the cases in which the U.S. Supreme Court (USSC) has
# substantively reviewed the constitutionality of a provision or application of a federal law

# Download data about
judicial_review <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/judicial_review_congress.csv")

### Data Manipulation

# Here is the codebook for the data: https://kewhitt.scholar.princeton.edu/sites/g/files/toruqf3716/files/kewhitt/files/judicial_review_of_congress_dataset_description_of_variables.pdf
# Create a dataset that includes the following variables:
# CASE, DATE, YEAR, EFFECT, S-DATE, TIME TO DECISION, CONGRESS, AREA1, ATT GEN, REVERSD, DISSENT
# Filter out cases that were announced before 1900

# For cases decided in the the 20th and 21st centuries:
# What is most common primary constitutional issue area considered at issue?
# What is the average time between the adoption of the statute by Congress (S-DATE) and the announcement of the decision (DATE)?
# What is the most dissenting votes that were cast in the case in which the USSC reversed the decision of the lower court?

### Visualizations

# Plot the number of cases reviewing Acts of Congress by decision year, using geom_line()
# Add a smoothed trend (geom_smooth())
# Create a second aligned plot (using patchwork) for the share of cases that invalidate statutes over time

# Which policy domains see more frequent judicial review? Is that stable over time?
# Hint: Create a line plot using geom_line() for # of cases by YEAR and facet by AREA1

# Using TIME TO DECISION, CONGRESS, DISSENT create a scatterplot matrix using GGally::ggpairs()
# Compute a correlation matrix for those same variables and visualize it with ggcorrplot() or corrgram()
# What you can see more quickly in the correlogram vs the scatterplot matrix?
# Which visualization would you include in a paper, and why?

# Let's create a model now of TIME TO DECISION, using EFFECT, CONGRESS, ATT GEN, and DISSENT as predictors
# Deliverables:
# One coefficient plot
# One marginal‑effects style plot (predicted values over a range)
# A short paragraph interpreting one key pattern and relating it to judicial politics