# Data Viz Final Project — Air_Quality (NYC), 2017
rm(list = ls())

# 0) Packages 
pkgs <- c("tidyverse", "scales", "ggrepel", "patchwork", "readxl")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------------
# 1) Paths 
data_path <- file.path("data", "Air_Quality.xlsx")

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# -----------------------------
# 2) Load data
# -----------------------------
aq_raw <- readxl::read_excel(data_path)

cat("Rows:", nrow(aq_raw), "Cols:", ncol(aq_raw), "\n")

# 3) Project scope 
ID_PM25 <- 365
ID_NO2  <- 375

TP_ANNUAL <- "Annual Average 2017"
TP_SUMMER <- "Summer 2017"
TP_WINTER <- "Winter 2016-17"
TP_WINTER_ALT <- "Winter 2017-18"  # optional

keep_tp <- c(TP_ANNUAL, TP_SUMMER, TP_WINTER)

aq <- aq_raw %>%
  filter(Measure == "Mean") %>%
  filter(`Indicator ID` %in% c(ID_PM25, ID_NO2)) %>%
  filter(`Time Period` %in% keep_tp) %>%
  mutate(
    `Data Value` = suppressWarnings(as.numeric(`Data Value`)),
    indicator = case_when(
      `Indicator ID` == ID_PM25 ~ "PM2.5",
      `Indicator ID` == ID_NO2  ~ "NO2",
      TRUE ~ "Other"
    ),
    time_period = factor(
      `Time Period`,
      levels = c(TP_WINTER, TP_SUMMER, TP_ANNUAL),
      labels = c("Winter (2016–17)", "Summer (2017)", "Annual avg (2017)")
    ),
    place = `Geo Place Name`
  ) %>%
  filter(!is.na(`Data Value`))

cat("After filtering:", nrow(aq), "rows\n")
cat("Time Period counts:\n")
print(table(aq$time_period, aq$indicator))

# 4) Figure 1 — Lollipop ranking (Annual avg 2017)
TOP_N <- 12

aq_annual <- aq %>%
  filter(time_period == "Annual avg (2017)") %>%
  group_by(indicator, place) %>%
  summarise(value = mean(`Data Value`), .groups = "drop")

top_places <- aq_annual %>%
  group_by(indicator) %>%
  slice_max(order_by = value, n = TOP_N, with_ties = FALSE) %>%
  ungroup()

p1 <- top_places %>%
  mutate(place = fct_reorder(place, value)) %>%
  ggplot(aes(x = value, y = place)) +
  geom_segment(aes(x = 0, xend = value, y = place, yend = place)) +
  geom_point(size = 2) +
  facet_wrap(~ indicator, scales = "free_x") +
  labs(
    title = "Figure 1. Highest-pollution neighborhoods (Annual average 2017)",
    x = "Mean concentration (units differ by pollutant)",
    y = NULL,
    caption = "Ranking based on Annual avg (2017)."
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(fig_dir, "fig1_lollipop_top_neighborhoods_annual2017.png"),
  plot = p1, width = 9, height = 5, dpi = 300
)

# 5) Figure 2 — Distribution by period (boxplot)
p2 <- aq %>%
  ggplot(aes(x = time_period, y = `Data Value`)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(
    title = "Figure 2. Seasonal vs annual distributions (2017 focus)",
    x = NULL,
    y = "Mean concentration (units differ by pollutant)",
    caption = "Annual averages may hide seasonal peaks and tail risk."
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(
  filename = file.path(fig_dir, "fig2_boxplot_distribution_by_period.png"),
  plot = p2, width = 9, height = 5, dpi = 300
)

# 6) Figure 3 — Relationship NO2 vs PM2.5 by period (scatter)
aq_wide <- aq %>%
  group_by(time_period, indicator, place) %>%
  summarise(value = mean(`Data Value`), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>%
  filter(!is.na(NO2), !is.na(`PM2.5`))

p3 <- aq_wide %>%
  ggplot(aes(x = NO2, y = `PM2.5`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ time_period) +
  labs(
    title = "Figure 3. Do neighborhoods with higher NO2 also have higher PM2.5?",
    x = "NO2 (ppb)",
    y = "PM2.5 (mcg/m³)",
    caption = "Descriptive relationship (not causal). Line is a linear fit."
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(fig_dir, "fig3_scatter_relationship_by_period.png"),
  plot = p3, width = 10, height = 4.5, dpi = 300
)

# 7) Figure 4 — Seasonal change (Winter vs Summer) paired plot
season_delta <- aq %>%
  filter(time_period %in% c("Winter (2016–17)", "Summer (2017)")) %>%
  group_by(indicator, place, time_period) %>%
  summarise(value = mean(`Data Value`), .groups = "drop") %>%
  pivot_wider(names_from = time_period, values_from = value) %>%
  mutate(delta = `Winter (2016–17)` - `Summer (2017)`)

TOP_DELTA <- 12
delta_top <- season_delta %>%
  group_by(indicator) %>%
  slice_max(order_by = abs(delta), n = TOP_DELTA, with_ties = FALSE) %>%
  ungroup()

delta_long <- delta_top %>%
  select(indicator, place, `Winter (2016–17)`, `Summer (2017)`) %>%
  pivot_longer(
    cols = c(`Winter (2016–17)`, `Summer (2017)`),
    names_to = "season",
    values_to = "value"
  ) %>%
  mutate(season = factor(season, levels = c("Summer (2017)", "Winter (2016–17)")))

p4 <- delta_long %>%
  ggplot(aes(x = season, y = value, group = place)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(
    title = "Figure 4. Neighborhoods with the largest seasonal shifts (Winter vs Summer)",
    x = NULL,
    y = "Mean concentration",
    caption = "Top neighborhoods by absolute (Winter - Summer) difference."
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(fig_dir, "fig4_paired_seasonal_shift.png"),
  plot = p4, width = 9, height = 5, dpi = 300
)

# -----------------------------
# 8) Done
# -----------------------------
cat("\nSaved figures to:", fig_dir, "\n")
cat(" - fig1_lollipop_top_neighborhoods_annual2017.png\n")
cat(" - fig2_boxplot_distribution_by_period.png\n")
cat(" - fig3_scatter_relationship_by_period.png\n")
cat(" - fig4_paired_seasonal_shift.png\n")