library(tidyverse)
library(scales)  # For nicer scales

set.seed(1234)  # Make all random draws the same
example_data <- tibble(x1 = rnorm(10000),
                       x2 = rnorm(10000),
                       y1 = sample(1:100, 10000, replace = TRUE),
                       y2 = sample(LETTERS[1:4], 10000, replace = TRUE),
                       y3 = sample(LETTERS[10:11], 10000, replace = TRUE),
                       year = sample(2010:2017, 10000, replace = TRUE)) |>
  arrange(y2, year)

my_beautiful_fancy_theme <- theme_minimal(base_family = "Source Sans Pro") +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        strip.text = element_text(family = "Source Sans Pro", face = "bold",
                                  size = rel(1.3)))

# Individual figures

## Figure 1: Lollipop chart
example_data_summarized <- example_data |>
  group_by(y2, y3) |>
  summarize(n = n())

figure1 <- ggplot(example_data_summarized, aes(x = n, y = fct_rev(y2), color = y3)) +
  geom_pointrange(aes(xmin = 0, xmax = n), position = position_dodge(width = 0.5),
                  linewidth = 1, fatten = 5) +
  labs(x = "Total number of things", y = NULL) +
  guides(color = guide_legend(title = NULL)) +
  scale_color_manual(values = c("#FF4266", "#82B09C")) +
  my_beautiful_fancy_theme + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(figure1, filename = "figures/figure1.pdf", device = cairo_pdf,
       width = 6, height = 4, units = "in", bg = "transparent")

## Figure 2: Changes over time
example_data_time <- example_data |>
  pivot_longer(cols = c(x1, x2), names_to = "x_names", values_to = "value") |>
  group_by(x_names, year, y2) |>
  summarize(x_avg = mean(value),
            error = sd(value) / sqrt(length(value))) |>
  ungroup() |>
  mutate(upper = x_avg + (1.96 * error),
         lower = x_avg - (1.96 * error)) |>
  mutate(x_names = recode(x_names, 
                          x1 = "X1 (average)",
                          x2 = "X2 (average)"))

figure2 <- ggplot(example_data_time, aes(x = year, y = x_avg, color = x_names)) +
  geom_hline(yintercept = 0, linewidth = 0.75, color = "#CC3340", linetype = "dotted") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = x_names, color = NULL), alpha = 0.2) +
  geom_line(linewidth = 1) + 
  scale_color_manual(values = c("#FA6900", "#69D1E8")) + 
  scale_y_continuous(labels = label_percent()) +
  guides(color = guide_legend(title = NULL), fill = "none") +
  labs(x = NULL, y = "Whatever this is measuring") +
  facet_wrap(~ y2, nrow = 1) + 
  my_beautiful_fancy_theme + 
  theme(panel.grid.minor = element_blank())

ggsave(figure2, filename = "figures/figure2.pdf", device = cairo_pdf,
       width = 16, height = 3, units = "in", bg = "transparent")

## Figure 3: Relationships

# There are a lot of points here and they're all random and pointless, so I
# simplify this graphic by just taking a subset of them
example_data_subset <- example_data |>
  sample_n(500)
  
figure3 <- ggplot(example_data_subset, aes(x = y1, y = x2, color = y2)) +
  geom_point(size = 1, alpha = 0.75) + 
  geom_smooth(method = "lm", color = "#85144A", linewidth = 2) +
  labs(x = "Some variable", y = "Some other variable") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_manual(values = c("#188146", "#004259", "#B00DC9", "#FFE01C")) +
  facet_wrap(~ y3) +
  my_beautiful_fancy_theme + 
  theme(panel.grid.minor = element_blank())

ggsave(figure3, filename = "figures/figure3.pdf", device = cairo_pdf,
       width = 6, height = 4, units = "in", bg = "transparent")
