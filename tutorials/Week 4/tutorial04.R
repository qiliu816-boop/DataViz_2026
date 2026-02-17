###############################
# Data Viz  
# Tutorial 4:  
# Themes & Annotations        
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
lapply(c("tidyverse", "ggplot2", "ggrepel", "extrafont", "WDI", "ggtext"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# Import dataset
CHES <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/1999-2024_CHES.csv")
# subset to only years 2010, 2014, 2019
CHES_subset <- CHES |> filter(year<2024 & year>2008)
# change level labels for govt variable
CHES_subset$govt <- as.factor(CHES_subset$govt)
levels(CHES_subset$govt) <- c("Party not in government","In government for part of year","Party in government full year")
# create base plot
base_plot <- ggplot(data=CHES_subset, aes(x=vote, y=lrgen,  color=as.factor(govt))) +
  geom_point() + facet_wrap(vars(year)) +
  labs(y="Left-right placement", x="Vote share (%)", color="Part of government")
pdf("CHES_base.pdf")
base_plot
dev.off()

pdf("CHES_minimal.pdf")
base_plot +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
dev.off()

base_plot <- base_plot +
  labs(title = "CHES Party Positions",
       subtitle = "2010 - 2019",
       caption = "Source: CHES Chapel Hill Expert Survey") + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold"))
base_plot
# need to use ggsave for special font to show up in pdf
ggsave("CHES_titles.pdf", base_plot, device = cairo_pdf)

base_plot <- base_plot + theme(
  # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
  strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
  # Bold axis titles
  axis.title = element_text(face = "bold"),
  # Add some space above the x-axis title and make it left-aligned
  axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
  # Add some space to the right of the y-axis title and make it top-aligned
  axis.title.y = element_text(margin = margin(r = 10), hjust = 1))
ggsave("CHES_axes.pdf", base_plot, device = cairo_pdf)


# Add a light grey background to the facet titles, with no borders
base_plot <- base_plot + theme(strip.background = element_rect(fill = "grey90", color = NA),
      # Add a thin grey border around all the plots to tie in the facet titles
      panel.border = element_rect(color = "grey90", fill = NA))
ggsave("CHES_facets.pdf", base_plot, device = cairo_pdf)

my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "grey90", fill = NA))


# subset to only Conservative parties
CHES_family <- CHES |> filter(family==1 | family==2 | family==4)
# change level labels for party family variable
CHES_family$family <- as.factor(CHES_family$family)
levels(CHES_family$family) <- c("Radical Right","Conservatives","Christian-Democratic")
CHES_family$country_name <- as.factor(CHES_family$country)
levels(CHES_family$country_name) <- c("BE", "DK", "GE", "GR", "ESP", "FR", "IRL", "IT", "NL", "UK", "POR", "AUS", "FIN", "SV", "BUL", "CZ", "EST", "HUN", "LAT", "LITH", "POL", "ROM", "SLO", "SLE", "CRO", "MAL", "LUX", "CYP")

family_plot <- ggplot(data = CHES_family, aes(x = year, y = vote, color = family)) +
  geom_line() + 
  facet_wrap(vars(country_name)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Year", y = "Vote (%)", color = "Party Family",
       title = "Percent of vote received by Conservative parties",
       subtitle = "Elections from 2000-2025",
       caption = "Source: CHES Chapel Hill Expert Survey") + 
  my_pretty_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
family_plot
ggsave("CHES_family.pdf", family_plot, device = cairo_pdf)

##############
### Group work
##############

# We'll be using cross-national data from the World Bank’s Open Data portal
# to graph rankings of CO2 emissions in 1995 and 2014

# subset data to the indicators we care about
indicators <- c(population = "SP.POP.TOTL",  # Population
                co2_emissions = "EN.GHG.ALL.MT.CE.AR5",  # CO2 emissions
                gdp_per_cap = "NY.GDP.PCAP.KD")  # GDP per capita
# download data from WDI package
wdi_co2 <- WDI(country = "all", indicators, extra = TRUE, 
                   start = 1995, end = 2015)
# clean the data by removing non-country countries
wdi_co2 <- wdi_co2 |> 
  filter(region != "Aggregates")

### Data Manipulation

# I'll do most of the data wrangling for you here:
co2_rankings <- wdi_co2 |> 
# Get rid of smaller countries
filter(population > 200000) |> 
  # Only look at two years
  filter(year %in% c(1995, 2014)) |> 
  # Get rid of all the rows that have missing values in co2_emissions
  drop_na(co2_emissions) |> 
  # Look at each year individually and rank countries based on their emissions that year
  group_by(year) |> 
  mutate(ranking = rank(co2_emissions)) |> 
  ungroup() |> 
  # Only select a handful of columns, mostly just the newly created "ranking"
  # column and some country identifiers
  select(iso3c, country, year, region, income, ranking) |> 
  # Right now the data is tidy and long, but we want to widen it and create
  # separate columns for emissions in 1995 and in 2014. pivot_wider() will make
  # new columns based on the existing "year" column (that's what `names_from`
  # does), and it will add "rank_" as the prefix, so that the new columns will
  # be "rank_1995" and "rank_2014". The values that go in those new columns will
  # come from the existing "ranking" column
  pivot_wider(names_from = year, names_prefix = "rank_", values_from = ranking) |> 
  # Find the difference in ranking between 2014 and 1995
  mutate(rank_diff = rank_2014 - rank_1995) |> 
  # Remove all rows where there's a missing value in the rank_diff column
  drop_na(rank_diff) |> 
  # Make an indicator variable that is true of the absolute value of the
  # difference in rankings is greater than 25. 25 is arbitrary here—that just
  # felt like a big change in rankings
  mutate(big_change = ifelse(abs(rank_diff) >= 25, TRUE, FALSE)) |> 
  # Make another indicator variable that indicates if the rank improved by a
  # lot, worsened by a lot, or didn't change much. We use the case_when()
  # function, which is like a fancy version of ifelse() that takes multiple
  # conditions. This is how it generally works:
  #
  # case_when(
  #  some_test ~ value_if_true,
  #  some_other_test ~ value_if_true,
  #  TRUE ~ value_otherwise
  #)
  mutate(better_big_change = case_when(
    rank_diff <= -25 ~ "Rank improved",
    rank_diff >= 25 ~ "Rank worsened",
    TRUE ~ "Rank changed a little"
  ))

### Visualizations

# These three functions make it so all geoms that use text, label, and label_repel 
# We'll use IBM Plex Sans as the font. 
# Those layers are *not* influenced by whatever you include in the base_family argument in something like theme_bw(), 
# So ordinarily you'd need to specify the font in each individual annotate(geom = "text") layer or geom_label() layer, and that's tedious
# This removes that tediousness
update_geom_defaults("text", list(family = "IBM Plex Sans"))
update_geom_defaults("label", list(family = "IBM Plex Sans"))
update_geom_defaults("label_repel", list(family = "IBM Plex Sans"))

# Plot the CO2 rankings with rank_1995 along the x-axis, and rank_2014 along the y-axis

# Add a reference line that goes from the bottom corner to the top corner

# Add points and color them by the type of change in rankings
# Use three different colors for the points (for "Rank changed a little", "Rank worsened", "Rank improved")

# Add repelled labels. Only use data where big_change is TRUE. Fill them by the type of change (so they match the color in geom_point() above) and use white text
# Use two different colors for the filled labels, with no labels for the "Rank changed a little"

# Add notes about what the outliers mean in the bottom left and top right corners
# Make these are italicized and light grey. The text in the bottom corner is justified to the right with hjust = 1, and the text in the top corner is justified to the left with hjust = 0

# Add mostly transparent rectangles in the bottom right and top left corners
# And, add text to define what the rectangles above actually mean

# Add arrows between the text and the rectangles
# These should use the segment geom, and the arrows should be added with the arrow() function

# Make the x and y axes expand all the way to the edges of the plot area and add breaks every 25 units from 0 to 175
 
# Add labels for x-axis, y-axis, plot title, subtitle, and caption
  
# Turn off the legends for color and fill, since the subtitle includes that

# Use theme_bw() with IBM Plex Sans
