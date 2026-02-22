#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "ggplot2", "ggridges", "ggrepel"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Data manipulation
#####################

### 1
# load data from URL
ces2015 <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/CES2015.csv")
# keep good-quality cases and main weight
ces2015 <- ces2015 |>
  filter(discard == "Good quality") |>
  # subset to necessary variables
  select(discard, age, p_voted, p_votechce, province, income_full, p_selfplace)

### 2 
ces2015 <- ces2015 |>
  filter(p_voted == "Yes" | p_voted=="No")

### 3
ces2015$age_num <- as.numeric(ces2015$age)
ces2015 <- ces2015 |>
  mutate(
    age_cat = case_when(
      2015 - age_num < 30 ~ "<30",
      2015 - age_num < 45 ~ "30-44",
      2015 - age_num < 65 ~ "45-64",
      2015 - age_num >= 65 ~ "65+"
    ),
    age_cat = factor(age_cat, levels = c("<30", "30-44", "45-64", "65+"))
  )

#####################
# Data visualization
#####################

### 1
pdf("plot1.pdf")
ggplot(ces2015 |> filter(!is.na(age_cat)),
  aes(x = age_cat, fill=p_voted)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position="dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Age group",
    y = "Proportion of respondents that voted",
    fill = "Voted"
    ) +
  theme_minimal()
dev.off()

pdf("plot2.pdf")
plot2_data <- ces2015[ces2015$p_votechce %in% c("Conservatives", "Liberal", "ndp", "Green Party", "Bloc Quebecois"),]
plot2_data <- plot2_data[plot2_data$p_selfplace!="" & plot2_data$p_selfplace!="1000",]
plot2_data$p_selfplace <- as.numeric(plot2_data$p_selfplace)
ggplot(plot2_data, 
       aes(x = p_selfplace, y=p_votechce, fill = p_votechce)) +
  geom_density_ridges(alpha = 0.5, scale = .9) +
  labs(
    x = "Left-right self-placement (0 = left, 10 = right)",
    y = "Density"
  ) + 
  lims(x=c(0,10))+
  theme_minimal() + theme(legend.position = "none")
dev.off()

# get only real income values
plot3_data <- ces2015[ces2015$income_full %in% c("less than $29,999", "more than $110,000", "between $60,000 and $89,999", "between $30,000 and $59,999", "between $90,000 and $109,999"),]
# make income_full a factor
plot3_data$income_full <- factor(plot3_data$income_full, levels = c("less than $29,999", "between $30,000 and $59,999", "between $60,000 and $89,999", "between $90,000 and $109,999", "more than $110,000"))
# change labels for income_full
levels(plot3_data$income_full) <- c("<$30k", "$30-60k", "$60-90k", "$90-110k", ">$110k")
# make province a factor
plot3_data$province <- as.factor(plot3_data$province)
# change labels for province
levels(plot3_data$province) <- c("Alberta", "BC", "Manitoba", "New Brunswick", "Newfoundland", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")
plot3 <- ggplot(plot3_data, aes(x = income_full, fill=p_voted)) +
  geom_histogram(stat="count", position="dodge") +
  facet_wrap(vars(province)) +
  labs(
    x = "Household income group",
    y = "Estimated turnout rate",
    fill="Voted",
    title = "Turnout by income and education, CES 2015"
  ) +
  theme_minimal() 

pdf("plot3.pdf")
plot3
dev.off()

# create own theme
theme_ces <- function() {
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = rel(1.7)),
      plot.subtitle = element_text(size = rel(1.3), colour = "grey40"),
      plot.caption = element_text(face = "italic", size = rel(0.8), colour = "grey50", hjust = 0),
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", hjust = 0),
      axis.title = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
      axis.title.y = element_text(margin = margin(r = 10), vjust = 1),
      strip.background = element_rect(fill = "grey90", colour = NA),
      panel.border = element_rect(colour = "grey90", fill = NA)
    )
}

# apply theme to previous plot
plot4 <- plot3 +
  labs(
    x = "Turnout among potential voters",
    y = NULL,
    title = "Turnout does not appear to be linked to income",
    subtitle = "Turnout by region in Canadian federal election",
    caption = "Source: CES 2015 combined file",
    tag = "*Turnout was strongest in Ontario and Quebec"
  ) +
  theme_ces() +
  coord_cartesian(ylim = c(0, 350), clip = "off") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        plot.tag.position = c(.7, .25))
  
# save as pdf
ggsave("plot4.pdf", plot4, device = cairo_pdf)
