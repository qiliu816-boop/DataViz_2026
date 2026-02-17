# ============================================================
# Problem Set 3 - Data Visualisation (CES 2015)
# Portable & robust script (runs on any computer)
# ============================================================

rm(list = ls())

# -----------------------------
# 0) Packages (auto-install)
# -----------------------------
pkgs <- c("tidyverse", "ggrepel", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------
# 1) Robust path: locate data
# -----------------------------
args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)])
if (length(script_path) == 0) {
  # If running interactively in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
  } else {
    stop("Cannot determine script path. Run with Source (RStudio) or: Rscript PS03.R")
  }
}
script_dir <- normalizePath(dirname(script_path))
data_path  <- file.path(script_dir, "data", "CES2015.csv")
if (!file.exists(data_path)) stop("Data file not found at: ", data_path)

# -----------------------------
# 2) Load data
# -----------------------------
ces_raw <- readr::read_csv(data_path, show_col_types = FALSE)

# -----------------------------
# 3) Helper functions
# -----------------------------
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    stop("Cannot find any of these columns: ", paste(candidates, collapse = ", "))
  }
  hit[1]
}

# Safe numeric conversion (returns NA for non-numeric)
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Standardize yes/no text
norm_txt <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x
}

# -----------------------------
# 4) Standardise key column names (robust to naming differences)
# -----------------------------
# These candidate lists cover most teaching / export variants
discard_col <- pick_col(ces_raw, c("discard", "Discard", "DISCARD"))
age_col     <- pick_col(ces_raw, c("age", "Age", "birth_year", "BirthYear", "yob", "YOB", "year_birth"))
prov_col    <- pick_col(ces_raw, c("province", "Province", "prov", "Prov", "PROV", "PROVINCE"))
voted_col   <- pick_col(ces_raw, c("p_voted", "p_votedlong", "voted", "Voted", "turnout"))
ideo_col    <- pick_col(ces_raw, c("p_selfplace", "selfplace", "ideology", "lrscale"))
party_col   <- pick_col(ces_raw, c("vote_for", "Vote_for", "voteint", "party", "intended_vote"))
income_col  <- pick_col(ces_raw, c("income_full", "Income_full", "income", "income_cat"))

ces_raw <- ces_raw %>%
  rename(
    discard     = all_of(discard_col),
    age         = all_of(age_col),
    province    = all_of(prov_col),
    p_voted     = all_of(voted_col),
    p_selfplace = all_of(ideo_col),
    vote_for    = all_of(party_col),
    income_full = all_of(income_col)
  )

# -----------------------------
# 5) Core cleaning 
# -----------------------------
# (A) Good quality filter: supports numeric coding OR text label
discard_txt <- norm_txt(ces_raw$discard)
discard_num <- to_num(ces_raw$discard)

good_quality <- dplyr::case_when(
  !is.na(discard_num) ~ discard_num == 0,
  discard_txt %in% c("good quality", "good_quality", "good") ~ TRUE,
  TRUE ~ FALSE
)

ces0 <- ces_raw %>%
  filter(good_quality)

if (nrow(ces0) == 0) {
  stop("After filtering for Good quality, dataset is empty. ",
       "Check 'discard' coding in your file with: table(ces_raw$discard, useNA='ifany')")
}

# (B) Turnout recode: supports numeric (1/5/8/9) OR text (yes/no/dk/refused)
voted_txt <- norm_txt(ces0$p_voted)
voted_num <- to_num(ces0$p_voted)

ces1 <- ces0 %>%
  mutate(
    turnout = case_when(
      # numeric CES coding
      !is.na(voted_num) & voted_num == 1 ~ 1,
      !is.na(voted_num) & voted_num == 5 ~ 0,
      !is.na(voted_num) & voted_num %in% c(8, 9) ~ NA_real_,
      # text variants
      voted_txt %in% c("yes", "y") ~ 1,
      voted_txt %in% c("no", "n") ~ 0,
      voted_txt %in% c("don't know", "dont know", "dk", "refused", "refuse") ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(turnout))

if (nrow(ces1) == 0) {
  stop("After recoding turnout (p_voted) and dropping missing, dataset is empty. ",
       "Inspect p_voted values with: table(ces0$p_voted, useNA='ifany')")
}

# (C) Age (birth year) -> age_years -> age_group
# Age column might contain non-numeric codes; coerce + range filter
ces2 <- ces1 %>%
  mutate(
    birth_year = to_num(age),
    # Reasonable range for birth year; adjust if needed
    birth_year = if_else(birth_year >= 1900 & birth_year <= 2005, birth_year, NA_real_),
    age_years  = 2015 - birth_year,
    age_group  = case_when(
      age_years < 30 ~ "<30",
      age_years >= 30 & age_years <= 44 ~ "30–44",
      age_years >= 45 & age_years <= 64 ~ "45–64",
      age_years >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("<30", "30–44", "45–64", "65+"))
  ) %>%
  filter(!is.na(age_group))

if (nrow(ces2) == 0) {
  stop("After computing age_group, dataset is empty. ",
       "Check age/birth year coding with: table(ces1$age, useNA='ifany')")
}

# (D) Province clean (robust: handles names, abbreviations, numeric codes)
prov_chr <- trimws(as.character(ces2$province))
prov_low <- tolower(prov_chr)

ces <- ces2 %>%
  mutate(
    province_clean = case_when(
      prov_low %in% c("1000", "999", "9999") ~ NA_character_,
      prov_low %in% c("on", "ontario") ~ "Ontario",
      prov_low %in% c("qc", "quebec") ~ "Quebec",
      prov_low %in% c("bc", "british columbia") ~ "British Columbia",
      prov_low %in% c("ab", "alberta") ~ "Alberta",
      prov_low %in% c("mb", "manitoba") ~ "Manitoba",
      prov_low %in% c("sk", "sask", "saskatchewan") ~ "Saskatchewan",
      prov_low %in% c("ns", "nova scotia") ~ "Nova Scotia",
      prov_low %in% c("nb", "new brunswick") ~ "New Brunswick",
      prov_low %in% c("pei", "pe", "prince edward island") ~ "Prince Edward Island",
      prov_low %in% c("nl", "nfld", "newfoundland", "newfoundland and labrador") ~ "Newfoundland & Labrador",
      prov_low %in% c("yt", "yukon") ~ "Yukon",
      prov_low %in% c("nt", "nwt", "northwest territories") ~ "Northwest Territories",
      prov_low %in% c("nu", "nunavut") ~ "Nunavut",
      TRUE ~ stringr::str_to_title(prov_low)  # fallback (also works for numeric codes as text)
    )
  )

# -----------------------------
# 6) Plot 1: Turnout rate by age group
# -----------------------------
turnout_by_age <- ces %>%
  group_by(age_group) %>%
  summarise(
    turnout_rate = mean(turnout, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

if (nrow(turnout_by_age) == 0) stop("turnout_by_age is empty; check age_group/turnout recoding.")

p1 <- ggplot(turnout_by_age, aes(x = age_group, y = turnout_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Turnout increases strongly with age",
    subtitle = "CES 2015 (Good quality). Turnout recoded from voting question (Yes=1, No=0; DK/Ref=missing).",
    x = NULL,
    y = "Turnout rate",
    caption = "Source: Canadian Election Study (CES) 2015. Unweighted. Age computed as 2015 - birth year."
  )

# -----------------------------
# 7) Plot 2: Ideology density by party (major parties)
# -----------------------------
# Party mapping (supports numeric codes 1-5 OR text labels)
party_num <- to_num(ces$vote_for)
party_txt <- norm_txt(ces$vote_for)

ces_ideo <- ces %>%
  mutate(
    selfplace = to_num(p_selfplace),
    party = case_when(
      !is.na(party_num) & party_num == 1 ~ "Liberal",
      !is.na(party_num) & party_num == 2 ~ "Conservative",
      !is.na(party_num) & party_num == 3 ~ "NDP",
      !is.na(party_num) & party_num == 4 ~ "Bloc Québécois",
      !is.na(party_num) & party_num == 5 ~ "Green",
      party_txt %in% c("liberal", "liberals") ~ "Liberal",
      party_txt %in% c("conservative", "conservatives") ~ "Conservative",
      party_txt %in% c("ndp", "new democratic party") ~ "NDP",
      party_txt %in% c("bloc", "bloc quebecois", "bloc québécois") ~ "Bloc Québécois",
      party_txt %in% c("green", "greens") ~ "Green",
      TRUE ~ NA_character_
    ),
    party = factor(party, levels = c("Conservative", "Liberal", "NDP", "Bloc Québécois", "Green"))
  ) %>%
  filter(!is.na(selfplace)) %>%
  filter(selfplace >= 0 & selfplace <= 10) %>%
  filter(!is.na(party))

p2 <- ggplot(ces_ideo, aes(x = selfplace, fill = party)) +
  geom_density(alpha = 0.35, adjust = 1.1) +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(
    title = "Ideological self-placement differs across intended vote",
    subtitle = "Restricted to non-missing left–right self-placement (0–10) and major parties only.",
    x = "Left–Right self-placement (0=Left, 10=Right)",
    y = "Density",
    caption = "Source: CES 2015. Sample: Good quality; DK/Ref treated as missing. Unweighted."
  ) +
  guides(fill = guide_legend(title = NULL))

# -----------------------------
# 8) Plot 3: Turnout counts by income_full, faceted by province
# -----------------------------
inc_num <- to_num(ces$income_full)
inc_txt <- norm_txt(ces$income_full)

ces_income <- ces %>%
  mutate(
    income_group = case_when(
      inc_txt %in% c("less than $29,999", "less than 29,999") ~ "1",
      inc_txt %in% c("between $30,000 and $59,999", "30,000-59,999") ~ "2",
      inc_txt %in% c("between $60,000 and $89,999", "60,000-89,999") ~ "3",
      inc_txt %in% c("between $90,000 and $109,999", "90,000-109,999") ~ "4",
      inc_txt %in% c("more than $110,000", "110,000+") ~ "5",
  
      !is.na(inc_num) & inc_num %in% 1:5 ~ as.character(inc_num),
      TRUE ~ NA_character_
    ),
    income_group = factor(
      income_group,
      levels = as.character(1:5),
      labels = c("<$30k", "$30–59k", "$60–89k", "$90–109k", ">$110k")
    ),
    turnout_f = factor(turnout, levels = c(0, 1), labels = c("Did not vote", "Voted"))
  ) %>%
  filter(!is.na(income_group)) %>%
  filter(!is.na(province_clean))

if (nrow(ces_income) == 0) {
  stop("ces_income is empty after filtering income_group and province_clean. ",
       "Check income_full/province values with:\n",
       "table(ces$income_full, useNA='ifany')\n",
       "table(ces$province, useNA='ifany')")
}

p3 <- ggplot(ces_income, aes(x = income_group, fill = turnout_f)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ province_clean) +
  labs(
    title = "Turnout by income group, by province",
    subtitle = "Income from income_full (5 groups). Turnout recoded from voting question; DK/Ref treated as missing.",
    x = "Household income group",
    y = "Count",
    caption = "Source: CES 2015. Sample: Good quality. Unweighted."
  ) +
  guides(fill = guide_legend(title = NULL)) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

# -----------------------------
# 9) Custom theme + final polished plot with ggrepel (Q4)
# -----------------------------
theme_ps3 <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9, color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA)
    )
}

gap_df <- turnout_by_age %>%
  summarise(
    min_rate = min(turnout_rate),
    max_rate = max(turnout_rate),
    min_group = age_group[which.min(turnout_rate)],
    max_group = age_group[which.max(turnout_rate)],
    .groups = "drop"
  )

p1_final <- ggplot(turnout_by_age, aes(x = age_group, y = turnout_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_ps3() +
  labs(
    title = "Older Canadians are far more likely to vote than younger adults",
    subtitle = "CES 2015 (Good quality). Age from birth year; turnout: Yes=1, No=0; DK/Ref=missing.",
    x = NULL,
    y = "Turnout rate",
    caption = "Source: Canadian Election Study (CES) 2015. Key coding: discard=Good quality; DK/Ref treated as missing. Unweighted."
  ) +
  geom_label_repel(
    data = turnout_by_age %>% filter(age_group %in% c(gap_df$min_group, gap_df$max_group)),
    aes(label = paste0(as.character(age_group), ": ", scales::percent(turnout_rate, accuracy = 1))),
    nudge_y = 0.08,
    show.legend = FALSE
  )

# -----------------------------
# 10) Save plots
# -----------------------------
fig_dir <- file.path(script_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

ggsave(file.path(fig_dir, "plot1_turnout_by_age.png"), p1, width = 7, height = 4, dpi = 300)
ggsave(file.path(fig_dir, "plot2_ideology_density_by_party.png"), p2, width = 8, height = 4.5, dpi = 300)
ggsave(file.path(fig_dir, "plot3_turnout_by_income_facet_province.png"), p3, width = 10, height = 7, dpi = 300)
ggsave(file.path(fig_dir, "plot4_final_polished.png"), p1_final, width = 7.5, height = 4.5, dpi = 300)

# Print to viewer
p1
p2
p3
p1_final

