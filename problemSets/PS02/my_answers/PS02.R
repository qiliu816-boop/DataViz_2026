# =========================
# Problem Set 2
# Data Visualisation for Social Scientists
# =========================

# --- Packages (portable) ---
pkgs <- c(
  "tidyverse", "readr", "dplyr", "ggplot2", "ggridges",
  "knitr", "scales"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))

# --- Folders ---
if (!dir.exists("outputs")) dir.create("outputs")

# --- Load data (relative path for portability) ---
ncss_raw <- readr::read_csv("data/NCSS_v1.csv", show_col_types = FALSE)

# drop unnamed index column if present (prevents "New names: ...1")
if ("...1" %in% names(ncss_raw)) ncss_raw <- dplyr::select(ncss_raw, -...1)

# --- Select required variables ---
ncss <- ncss_raw %>%
  dplyr::select(
    CASEID,
    YEAR,
    GDREGION,
    NUMOFFMBR,
    TRAD6,
    TRAD12,
    INCOME
  )

# --- Filter to Christian / Jewish / Muslim congregations (TRAD6) ---
# NOTE: TRAD6 labels are in French in this dataset
ncss_cjm <- ncss %>%
  dplyr::filter(TRAD6 %in% c("Chrétiennes", "Juives", "Musulmanes"))

print(dplyr::count(ncss_cjm, TRAD6))

# English labels for plots only
ncss_cjm <- ncss_cjm %>%
  dplyr::mutate(
    TRAD6_EN = dplyr::case_when(
      TRAD6 == "Chrétiennes" ~ "Christian",
      TRAD6 == "Juives" ~ "Jewish",
      TRAD6 == "Musulmanes" ~ "Muslim",
      TRUE ~ TRAD6
    )
  )

# =========================
# Data Manipulation
# =========================

# --- Q3: counts + mean/median INCOME by YEAR and TRAD6 ---
summary_a3 <- ncss_cjm %>%
  dplyr::group_by(YEAR, TRAD6) %>%
  dplyr::summarise(
    n_congregations = dplyr::n(),
    mean_income = mean(INCOME, na.rm = TRUE),
    median_income = median(INCOME, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_a3)

# --- Export summary table for LaTeX ---
summary_tex <- knitr::kable(
  summary_a3,
  format = "latex",
  booktabs = TRUE,
  digits = 0,
  caption = "Counts and income summaries by year and TRAD6 (Christian/Jewish/Muslim only).",
  label = "tab:summary_a3"
)
writeLines(summary_tex, "outputs/summary_a3.tex")

# --- Q4: Create AVG_INCOME relative to yearly mean INCOME ---
year_income_avg <- ncss_cjm %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(
    year_avg_income = mean(INCOME, na.rm = TRUE),
    .groups = "drop"
  )

ncss_cjm <- ncss_cjm %>%
  dplyr::left_join(year_income_avg, by = "YEAR") %>%
  dplyr::mutate(
    # IMPORTANT: "Above average or average income" => >=
    AVG_INCOME = dplyr::if_else(INCOME >= year_avg_income, 1L, 0L),
    AVG_INCOME_LABEL = dplyr::if_else(
      AVG_INCOME == 1L,
      "Above or equal to yearly mean",
      "Below yearly mean"
    )
  )

# =========================
# Missingness / N reporting
# =========================
n_2009_total <- ncss_cjm %>%
  dplyr::filter(YEAR == 2009) %>%
  dplyr::summarise(N = dplyr::n())

n_2022_total <- ncss_cjm %>%
  dplyr::filter(YEAR == 2022) %>%
  dplyr::summarise(N = dplyr::n())

n_2022_income_nonmiss <- ncss_cjm %>%
  dplyr::filter(YEAR == 2022) %>%
  dplyr::summarise(N_income_nonmissing = sum(!is.na(INCOME)))

n_2022_members_nonmiss <- ncss_cjm %>%
  dplyr::filter(YEAR == 2022) %>%
  dplyr::summarise(N_NUMOFFMBR_nonmissing = sum(!is.na(NUMOFFMBR)))

print(n_2009_total)
print(n_2022_total)
print(n_2022_income_nonmiss)
print(n_2022_members_nonmiss)

n_table <- dplyr::bind_rows(
  dplyr::tibble(stat = "N (2009, CJM)", value = n_2009_total$N),
  dplyr::tibble(stat = "N (2022, CJM)", value = n_2022_total$N),
  dplyr::tibble(stat = "N with INCOME (2022)", value = n_2022_income_nonmiss$N_income_nonmissing),
  dplyr::tibble(stat = "N with NUMOFFMBR (2022)", value = n_2022_members_nonmiss$N_NUMOFFMBR_nonmissing)
)

writeLines(
  knitr::kable(n_table, format = "latex", booktabs = TRUE, col.names = c("Statistic", "Value")),
  "outputs/n_table.tex"
)

# Helper: SI number labels (replaces deprecated label_number_si())
si_labels <- scales::label_number(scale_cut = scales::cut_si(" "))

# =========================
# Data Visualisation
# =========================

# --- B1: Proportion above/below yearly mean income by TRAD12, faceted by YEAR ---
# Improve readability: flip coordinates (TRAD12 labels can be long)
p_b1 <- ggplot(ncss_cjm, aes(x = TRAD12, fill = AVG_INCOME_LABEL)) +
  geom_bar(position = "fill") +
  facet_wrap(~ YEAR) +
  coord_flip() +
  labs(
    title = "Proportion of congregations above/below yearly mean income",
    x = "TRAD12",
    y = "Proportion",
    fill = "Income category"
  ) +
  theme_minimal()

print(p_b1)
ggsave("outputs/fig_b1.png", p_b1, width = 10, height = 7, dpi = 300)

# --- Restrict to 2022 for analyses involving NUMOFFMBR ---
# NOTE (per instructor update): NUMOFFMBR is missing for all congregations in 2009
ncss_2022 <- ncss_cjm %>%
  dplyr::filter(YEAR == 2022)

# --- B2: Column chart (geom_col) of total official members by TRAD12 within TRAD6 (2022) ---
plot_b2_df <- ncss_2022 %>%
  dplyr::group_by(TRAD6_EN, TRAD12) %>%
  dplyr::summarise(
    total_members = sum(NUMOFFMBR, na.rm = TRUE),
    .groups = "drop"
  )

p_b2 <- ggplot(plot_b2_df, aes(x = TRAD12, y = total_members, fill = TRAD12)) +
  geom_col(position = "dodge") +
  facet_wrap(~ TRAD6_EN) +
  coord_flip() +
  scale_y_continuous(labels = si_labels) +
  labs(
    title = "Total official members by TRAD12 within TRAD6 (2022)",
    x = "Religious Classification (TRAD12)", 
    y = "Total official members"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8) 
  )

print(p_b2)
ggsave("outputs/fig_b2.png", p_b2, width = 12, height = 7, dpi = 300)

# --- B3: Ridge plot of INCOME by region (2022) ---
# Remove missing INCOME for ridge density estimation
ncss_2022_income <- ncss_2022 %>% dplyr::filter(!is.na(INCOME))

p_b3 <- ggplot(ncss_2022_income, aes(x = INCOME, y = GDREGION)) +
  geom_density_ridges(scale = 1.1, rel_min_height = 0.01, fill = "skyblue", alpha = 0.7) + 
  scale_x_continuous(labels = si_labels, limits = c(0, 5000000)) + 
  labs(
    title = "Distribution of yearly income by region (2022)",
    x = "Annual Income (CHF)",
    y = "Region (GDREGION)"
  ) +
  theme_minimal()

print(p_b3)
ggsave("outputs/fig_b3.png", p_b3, width = 10, height = 6, dpi = 300)

# --- B4: Boxplot of NUMOFFMBR by TRAD6, faceted by region (2022) ---
p_b4 <- ggplot(ncss_2022, aes(x = TRAD6_EN, y = NUMOFFMBR)) +
  geom_boxplot(outlier.alpha = 0.2, na.rm = TRUE, fill = "lightgrey") +
  facet_wrap(~ GDREGION) +
  coord_cartesian(ylim = c(0, 10000)) + 
  scale_y_continuous(labels = si_labels) +
  labs(
    title = "Official members per congregation by religion and region (2022)",
    subtitle = "Zoomed to 0-10k range for clarity",
    x = "Religious Tradition",
    y = "Number of Members"
  ) +
  theme_minimal()

print(p_b4)
ggsave("outputs/fig_b4.png", p_b4, width = 12, height = 8, dpi = 300)


