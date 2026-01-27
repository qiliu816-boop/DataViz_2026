############################################################
# 1. Environment & Package Setup
############################################################

packages <- c("tidyverse", "readxl", "janitor")

for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

############################################################
# 2. Data Loading & Initial Cleaning
############################################################

# EP1 MEP characteristics
mep_ep1 <- read_excel("mep_info_26Jul11.xls", sheet = "EP1") %>%
  clean_names() %>%
  rename(
    mepid = mep_id,
    epg   = ep_group,
    ms    = member_state,
    np    = national_party
  ) %>%
  mutate(
    nom_d1 = readr::parse_number(as.character(nom_d1)),
    nom_d2 = readr::parse_number(as.character(nom_d2))
  )

# EP1 roll-call votes (comma-separated)
rcv_ep1 <- readr::read_delim("rcv_ep1.txt", delim = ",", show_col_types = FALSE) %>%
  clean_names()

############################################################
# 3. Data Reshaping (Wide to Long) + Merge
############################################################

id_cols   <- c("mepid", "mepname", "ms", "np", "epg")
vote_cols <- setdiff(names(rcv_ep1), id_cols)

votes_long <- rcv_ep1 %>%
  pivot_longer(
    cols = all_of(vote_cols),
    names_to = "vote_id",
    values_to = "decision"
  )

# Merge votes (MEP-vote level) with MEP characteristics (MEP level)
# Use suffixes to avoid confusion, then harmonise column names.
mep_votes_ep1 <- votes_long %>%
  left_join(mep_ep1, by = "mepid", suffix = c("", "_mep")) %>%
  # Keep EP group from the vote file (epg), drop the EP group from mep file
  select(-epg_mep) %>%
  # Drop the unlabelled / invalid group if required by the analysis
  filter(epg != "0")

# Recode voting decisions into categories (Q3)
mep_votes_a5 <- mep_votes_ep1 %>%
  mutate(
    decision_label = case_when(
      decision == 1 ~ "Yes",
      decision == 2 ~ "No",
      decision == 3 ~ "Abstain",
      TRUE ~ "Absent/Other"
    ),
    is_valid = if_else(decision_label %in% c("Yes", "No", "Abstain"), 1L, 0L)
  )


############################################################
# 4. A5: Analysis & Statistics
############################################################

# Recode decisions:
# 1 = Yes, 2 = No, 3 = Abstain; others treated as Absent/Other (excluded from denominator).
mep_votes_a5 <- mep_votes_ep1 %>%
  mutate(
    decision_label = case_when(
      decision == 1 ~ "Yes",
      decision == 2 ~ "No",
      decision == 3 ~ "Abstain",
      TRUE ~ "Absent/Other"
    ),
    is_valid = if_else(decision_label %in% c("Yes", "No", "Abstain"), 1L, 0L)
  )

# A5-1 & A5-2: Mean Yes rate and mean Abstention rate by EP group
a5_analysis <- mep_votes_a5 %>%
  group_by(epg) %>%
  summarise(
    total_valid = sum(is_valid, na.rm = TRUE),
    mean_yes_rate = if_else(total_valid > 0, sum(decision == 1, na.rm = TRUE) / total_valid, NA_real_),
    mean_abstention_rate = if_else(total_valid > 0, sum(decision == 3, na.rm = TRUE) / total_valid, NA_real_),
    .groups = "drop"
  )

# A5-3: Mean NOMINATE positions by EP group (MEP-level table)
a5_nominate <- mep_ep1 %>%
  filter(epg != "0") %>%
  group_by(epg) %>%
  summarise(
    mean_nom_d1 = mean(nom_d1, na.rm = TRUE),
    mean_nom_d2 = mean(nom_d2, na.rm = TRUE),
    n_meps = n(),
    .groups = "drop"
  )

# Final A5 summary table
a5_final_table <- left_join(a5_analysis, a5_nominate, by = "epg")
print(a5_final_table)

readr::write_csv(a5_final_table, "a5_final_table.csv")

#  Summary table of decision categories across all votes
decision_counts <- mep_votes_a5 %>%
  count(decision_label, sort = TRUE)

print(decision_counts)
readr::write_csv(decision_counts, "decision_counts.csv")
############################################################
# 5. B1-B3: Visualizations 
############################################################

plot_data <- mep_ep1 %>% filter(epg != "0")

# B1: Density of NOMINATE Dimension 1
p_b1 <- ggplot(plot_data, aes(x = nom_d1, fill = epg)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Distribution of NOMINATE Dimension 1 by EP Group (EP1)",
    x = "NOMINATE Dimension 1 (nom_d1)",
    y = "Density",
    fill = "EP Group"
  ) +
  theme_minimal()

ggsave("fig_b1_density.png", plot = p_b1, width = 8, height = 4.5, dpi = 300)

# B2: Scatterplot NOMINATE D1 vs D2
p_b2 <- ggplot(plot_data, aes(x = nom_d1, y = nom_d2, color = epg)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "NOMINATE Dimension 1 vs Dimension 2 by EP Group (EP1)",
    x = "NOMINATE Dimension 1 (nom_d1)",
    y = "NOMINATE Dimension 2 (nom_d2)",
    color = "EP Group"
  ) +
  theme_minimal()

ggsave("fig_b2_scatter.png", plot = p_b2, width = 8, height = 5.5, dpi = 300)

# B3: Cohesion (MEP-level Yes proportion boxplot)
mep_yes_prop <- mep_votes_a5 %>%
  filter(decision %in% c(1, 2, 3)) %>%
  group_by(mepid, epg) %>%
  summarise(
    yes_prop = sum(decision == 1, na.rm = TRUE) / n(),
    .groups = "drop"
  )

p_b3 <- ggplot(mep_yes_prop, aes(x = epg, y = yes_prop)) +
  geom_boxplot() +
  labs(
    title = "Proportion Voting Yes by EP Group (Cohesion) - EP1",
    x = "EP Group",
    y = "Yes proportion (Yes / (Yes + No + Abstain))"
  ) +
  theme_minimal()

ggsave("fig_b3_boxplot.png", plot = p_b3, width = 8, height = 4.5, dpi = 300)

summary(mep_yes_prop$yes_prop)




library(lubridate)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)

vote_info_ep1 <- read_excel("vote_info_Jun2010.xls", sheet = "EP1") %>%
  clean_names()

date_col <- intersect(names(vote_info_ep1), c("vote_date", "date"))[1]
if (is.na(date_col)) stop("Cannot find a date column in vote_info_ep1. Check names(vote_info_ep1).")

vote_year_lookup <- vote_info_ep1 %>%
  mutate(
    vote_date_raw = .data[[date_col]],
    vote_date_chr = str_trim(as.character(vote_date_raw)),
    vote_date_num = suppressWarnings(as.numeric(vote_date_chr)),
    
    # Step A: Excel serial date -> Date (safe)
    vote_date_from_num = if_else(
      !is.na(vote_date_num),
      as.Date(vote_date_num, origin = "1899-12-30"),
      as.Date(NA)
    ),
    
    # Step B: parse date strings -> Date (safe; failed parses become NA, no error)
    vote_date_from_chr = as.Date(
      suppressWarnings(lubridate::parse_date_time(
        str_sub(vote_date_chr, 1, 19),
        orders = c("Ymd", "Y-m-d", "dmy", "d/m/Y", "m/d/Y", "Y/m/d", "d.m.Y", "Ymd HMS", "Y-m-d HMS")
      ))
    ),
    
    # Combine: prefer numeric-derived dates, else use parsed string dates
    vote_date = coalesce(vote_date_from_num, vote_date_from_chr),
    
    vote_id = paste0("v", row_number()),
    year = year(vote_date)
  ) %>%
  select(vote_id, year)

cat("vote_year_lookup rows:", nrow(vote_year_lookup), "\n")
cat("Missing year:", sum(is.na(vote_year_lookup$year)), "\n")
cat("Unique years:", paste(sort(unique(vote_year_lookup$year)), collapse = ", "), "\n")

# 1) Join year back
votes_with_year <- votes_long %>%
  left_join(vote_year_lookup, by = "vote_id")

# 2) Map decision codes to labels (most common coding: 1=Yes, 2=No, 3=Abstain)
votes_valid_year <- votes_with_year %>%
  mutate(
    decision_label = case_when(
      decision == 1 ~ "Yes",
      decision == 2 ~ "No",
      decision == 3 ~ "Abstain",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(year)) %>%
  filter(decision_label %in% c("Yes", "No", "Abstain"))

# quick check
table(votes_valid_year$decision_label, useNA = "ifany")
table(votes_valid_year$year)

top_np <- votes_valid_year %>%
  count(np, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(np)

b4_data <- votes_valid_year %>%
  mutate(
    np_plot = if_else(
      as.character(np) %in% as.character(top_np),
      as.character(np),
      "Other"
    )
  ) %>%
  group_by(year, np_plot) %>%
  summarise(
    yes_prop = mean(decision_label == "Yes"),
    .groups = "drop"
  )

p_b4 <- ggplot(b4_data, aes(x = factor(year), y = yes_prop, fill = np_plot)) +
  geom_col(position = "dodge") +
  labs(
    title = "B4: Proportion Voting Yes per Year by National Party",
    x = "Year",
    y = "Yes proportion",
    fill = "National Party"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_b4)
ggsave("fig_b4.png", plot = p_b4)

b5_data <- votes_valid_year %>%
  filter(epg != "0", !is.na(epg)) %>%
  group_by(year, epg) %>%
  summarise(
    avg_yes_share = mean(decision_label == "Yes"),
    .groups = "drop"
  )

p_b5 <- ggplot(b5_data, aes(x = year, y = avg_yes_share, color = epg, group = epg)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "B5: Average Yes Share per Year by EP Group",
    x = "Year",
    y = "Average Yes share",
    color = "EP Group"
  ) +
  theme_minimal()

print(p_b5)
ggsave("fig_b5.png", plot = p_b5)







