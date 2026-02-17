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

lapply(c("tidyverse", "ggplot2", "ggridges"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Data manipulation
#####################

### 1
# load data from URL
NCSS <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/DataViz_2026/refs/heads/main/datasets/NCSS_v1.csv")
# subset to selected variables
NCSS <- NCSS |>
  select(
    CASEID,      # Congregation ID
    YEAR,        # Survey year
    GDREGION,    # Region
    NUMOFFMBR,   # Number of official members
    TRAD6,       # 6-level religion
    TRAD12,      # 12-level religion
    INCOME       # Total income last fiscal year
  )

### 2
# filter to Christian, Jewish, and Muslim congregations 
NCSS <- NCSS |>
  filter(TRAD6 %in% c("Chrétiennes", "Juives", "Musulmanes"))

### 3
NCSS |>
  group_by(TRAD6, YEAR) |>
  summarise(
    n_congregations = n(),
    mean_income    = mean(INCOME, na.rm = TRUE),
    median_income  = median(INCOME, na.rm = TRUE),
    .groups = "drop"
  )

### 4
NCSS <- NCSS |>
  group_by(YEAR) |>
  mutate(
    year_mean_income = mean(INCOME, na.rm = TRUE),
    AVG_INCOME   = if_else(
      INCOME >= year_mean_income, "Above Average", "Below Average"
    )
  ) 

#####################
# Data manipulation
#####################

### 1
pdf("plot1.pdf")
NCSS$AVG_INCOME <- as.factor(NCSS$AVG_INCOME)
ggplot(NCSS |>
         group_by(YEAR, TRAD12, AVG_INCOME) |>
         summarise(n = n()) |>
         mutate(prop = n / sum(n)), 
        aes(x = TRAD12, y = prop, fill = AVG_INCOME)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "12-level religious classification (TRAD12)",
       y = "Proportion of congregations", 
       fill = "Income category") +
  facet_wrap(~ YEAR) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

### 2
pdf("plot2.pdf", width=10)
NCSS_2022 <- NCSS |> filter(YEAR == 2022)
ggplot(NCSS_2022 |>
        group_by(TRAD6, TRAD12) |>
        summarise(total_members = sum(NUMOFFMBR, na.rm = TRUE)),
      aes(x = TRAD6, y = total_members, fill = TRAD12)) +
  geom_col(width = 0.5, position = position_dodge(width=1)) +
  labs(x = "Religious classification (TRAD6)",
       y = "Total number of official members\n", 
       fill = "12-level Religious Classification (TRAD12)") +
  guides(fill=guide_legend(ncol=3))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.position = "bottom", 
        axis.title = element_text(size=14))
dev.off()

### 3
pdf("plot3.pdf")
ggplot(NCSS_2022, aes(x = INCOME, y = GDREGION, fill = GDREGION)) +
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1) +
  labs(x = "Total income in last fiscal year (INCOME)",
       y = "Region (GDREGION)") +
  theme_bw() +
  lims(x=c(0, 9000000))+
  theme(legend.position = "none")
dev.off()

pdf("plot4.pdf")
ggplot(NCSS_2022, aes(x = TRAD6, y = NUMOFFMBR)) +
  geom_boxplot() +
  labs(x = "6-level religious classification (TRAD6)",
       y = "Number of official members per congregation (NUMOFFMBR)") +
  facet_wrap(~ GDREGION) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
