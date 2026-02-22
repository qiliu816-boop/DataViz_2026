###############################
# Data Viz  
# Tutorial 5:  
# Space, Time, & Text     
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
lapply(c("tidyverse", "ggplot2", "ggrepel", "sf", "WDI", "regions", "tidytext", "gutenbergr"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#################
### In class demo
#################

# Download "Admin 0 – Countries" from
# https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
world_map <- read_sf("~/Downloads/110m_cultural/ne_110m_admin_0_countries.shp")
# remove Antarctica
world_map <- world_map |> filter(ISO_A3 != "ATA")
# take a look at basic world map
ggplot() + 
  geom_sf(data = world_map)

# Natural Earth dataset happens to come with some columns with a coloring scheme with 7–13 colors (MAPCOLOR7, MAPCOLOR9, etc.) 
# so we can make a map where no countries with a shared border share a color
ggplot() + 
  geom_sf(data = world_map, 
          aes(fill = as.factor(MAPCOLOR7)),
          color = "#401D16", linewidth = 0.25) +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = "none") +
  theme_void()

# we can also make maps with different "projections"
ggplot() + 
  geom_sf(data = world_map, 
          fill = "#669438", color = "#32481B", linewidth = 0.25) +
  coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
  # Or use the name instead of the number
  # coord_sf(crs = "+proj=robin")
  theme_void()

# or Mercator
ggplot() + 
  geom_sf(data = world_map, 
          fill = "#669438", color = "#32481B", linewidth = 0.25) +
  coord_sf(crs = st_crs("EPSG:3395")) +  # Mercator
  # Or use the name instead of the number
  # coord_sf(crs = "+proj=merc")
  theme_void()

# We can also make a choropleth map of life expectancy with data from the World Bank
# So, import dataset
# get data from the World Bank
# including life expectancy, internet users
indicators <- c(life_expectancy = "SP.DYN.LE00.IN", internet_users = "IT.NET.USER.ZS")
# just get data from 2020
wdi_raw <- WDI(country = "all", indicators, extra = TRUE, 
               start = 1994, end = 2024)
# now we need to add them together
# so we basically append the World Bank data to the end of the world shapefiles and line up rows that have matching ISO3 codes
# ISO3 column is named ISO_A3 in the shapefile data, and it’s named iso3c in the WDI data, so we tell left_join() that those are the same column
world_map_life_expectancy <- world_map |> 
  # focus on 2020 and life expectancy
  left_join(wdi_raw |> select(life_expectancy, year, iso3c) |> filter(year==2020), by = c("ISO_A3" = "iso3c"))

ggplot() + 
  geom_sf(data = world_map_life_expectancy, 
          aes(fill = life_expectancy),
          linewidth = 0.25) +
  coord_sf(crs = st_crs("ESRI:54030")) +  # Robinson
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "Life expectancy") +
  theme_void() +
  theme(legend.position = "bottom")

# notice that France and Norway are grayed out because they’re missing data
# that’s because the ISO_A3 code in the Natural Earth data is missing for both France and Norway

# now let's look at one country's internet users over time, for instance IRE
ggplot(wdi_raw |> filter(country=="Ireland"), aes(x = year, y = internet_users)) +
  geom_line()

# now let's clean these up a little
# first we can change the labels and themes and colors:
ggplot(wdi_raw |> filter(country=="Ireland"), aes(x = year, y = internet_users)) +
  geom_line(color = "#0074D9", linewidth = 1) +
  labs(y = "% of population",
       x = NULL,
       title = "Individuals using the internet",
       subtitle = "Yearly data",
       caption = "Source: World Bank") +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

# let's add some historical time periods
# In 1998, the Internet Service Providers Association of Ireland (ISPAI) listed only 24 Internet access and hosting providers
# Eir, the largest telephone company in Ireland, began rolling out broadband Internet access in 2002
# In August 2012, Pat Rabbitte, the Minister for Communications, Energy and Natural Resources, outlined a national broadband plan
internet_dates <- data.frame("start"=c(1998, 2002, 2012), "end"=c(1999, 2003, 2013))

ggplot(wdi_raw |> filter(country=="Ireland"), aes(x = year, y = internet_users)) +
  geom_rect(data = internet_dates, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(color = "#0074D9", linewidth = 1) +
  labs(y = "% of population",
       x = NULL,
       title = "Individuals using the internet",
       subtitle = "Yearly data",
       caption = "Source: World Bank") +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

# we can even add extra annotations describing these spikes
ggplot(wdi_raw |> filter(country=="Ireland"), aes(x = year, y = internet_users)) +
  geom_rect(data = internet_dates, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#B10DC9", alpha = 0.3) +
  geom_line(color = "#0074D9", linewidth = 1) +
  annotate(geom = "label", x = 1996, y = 50, 
           label = "ISPAI Founded", size = 3, family = "Roboto Condensed") +
  annotate(geom = "label", x = 2006, y = 15, 
           label = "Eir Broadband Introduced", size = 3, family = "Roboto Condensed") +
  annotate(geom = "label", x = 2016, y = 30, 
           label = "National Broadband Plan", size = 3, family = "Roboto Condensed") +
  labs(y = "% of population",
       x = NULL,
       title = "Individuals using the internet",
       subtitle = "Yearly data",
       caption = "Source: World Bank") +
  theme_bw(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(face = "bold"))

# let's work with some text now
# we'll download some Shakespeare from Project Gutenberg
# 1524 - Hamlet
# 1532 - King Lear
# 1533 - Macbeth
# 1513 - Romeo and Juliet
tragedies_raw <- gutenberg_download(c(1524, 1532, 1533, 1513),
                                    meta_fields = "title")

# tokens, word counts, and single words
# let's visualize word frequencies and find the most common words
tragedies_words <- tragedies_raw |> 
  drop_na(text) |> 
  unnest_tokens(output = word, input = text)

# now, we can remove all common stop words like “a” and “the” that are listed in the stop_words dataset that is loaded when you load {tidytext}
# count how many times each word appears in each title/play
# and only keep the top 15 words
top_words_tragedies <- tragedies_words |> 
  # Remove stop words
  anti_join(stop_words, by = join_by(word)) |> 
  # Get rid of old timey words and stage directions
  filter(!(word %in% c("thou", "thy", "haue", "thee", 
                       "thine", "enter", "exeunt", "exit"))) |> 
  # Count all the words in each play
  count(title, word, sort = TRUE) |> 
  # Keep top 15 in each play
  group_by(title) |> 
  slice_max(n, n = 15) |> 
  ungroup() |> 
  # Make the words an ordered factor so they plot in order
  mutate(word = fct_inorder(word))

ggplot(top_words_tragedies, aes(y = fct_rev(word), x = n, fill = title)) + 
  geom_col() + 
  guides(fill = "none") +
  labs(y = "Count", x = NULL, 
       title = "15 most frequent words in four Shakespearean tragedies") +
  facet_wrap(vars(title), scales = "free_y") +
  theme_bw()

# can also look at pairs of words instead of single words
tragedies_bigrams <- tragedies_raw |> 
  drop_na(text) |> 
  # n = 2 here means bigrams. We could also make trigrams (n = 3) or any type of n-gram
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) |> 
  # Get rid of NAs in the new bigram column
  drop_na(bigram) |> 
  # Split the bigrams into two words so we can remove stopwords
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) |> 
  filter(!word1 %in% c("thou", "thy", "thine", "enter", "exeunt", "exit"),
         !word2 %in% c("thou", "thy", "thine", "enter", "exeunt", "exit")) |> 
  # Put the two word columns back together
  unite(bigram, word1, word2, sep = " ")

top_bigrams <- tragedies_bigrams |> 
  # Count all the bigrams in each play
  count(title, bigram, sort = TRUE) |> 
  # Keep top 15 in each play
  group_by(title) |> 
  slice_max(n, n = 10) |> 
  ungroup() |> 
  # Make the bigrams an ordered factor so they plot in order
  mutate(bigram = fct_inorder(bigram))

ggplot(top_bigrams, aes(y = fct_rev(bigram), x = n, fill = title)) + 
  geom_col() + 
  guides(fill = "none") +
  labs(y = "Count", x = NULL, 
       title = "15 most frequent bigrams in four Shakespearean tragedies") +
  facet_wrap(vars(title), scales = "free") +
  theme_bw()

# Term frequency-inverse document frequency (tf-idf)
# we can determine which words are the most unique for each book/document in our corpus 
# we need to calculate the tf-idf (term frequency-inverse document frequency) score for each term
tragedy_words <- tragedies_raw |> 
  drop_na() |> 
  # Split into word tokens
  unnest_tokens(output = word, input = text) |> 
  # Remove stop words and old timey words
  anti_join(stop_words, by = join_by(word)) |> 
  filter(!word %in% c("thou", "thy", "haue", "thee", 
                      "thine", "enter", "exeunt", "exit")) |> 
  count(title, word, sort = TRUE)

# Add the tf-idf values to the counts
tragedy_tf_idf <- tragedy_words |> 
  bind_tf_idf(word, title, n)

# Get the top 10 uniquest words
tragedy_tf_idf_plot <- tragedy_tf_idf |> 
  arrange(desc(tf_idf)) |> 
  group_by(title) |> 
  slice_max(tf_idf, n = 10) |> 
  ungroup() |> 
  mutate(word = fct_inorder(word))

ggplot(tragedy_tf_idf_plot, 
       aes(y = fct_rev(word), x = tf_idf, fill = title)) +
  geom_col() +
  guides(fill = "none") +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(vars(title), scales = "free") +
  theme_bw()

# we can also do sentiment analysis
tragedy_words <- tragedies_raw |> 
  drop_na() |> 
  # Split into word tokens
  unnest_tokens(output = word, input = text) |> 
  # Remove stop words and old timey words
  anti_join(stop_words, by = join_by(word)) |> 
  filter(!word %in% c("thou", "thy", "haue", "thee", 
                      "thine", "enter", "exeunt", "exit"))

# Join the sentiment dictionary 
tragedy_sentiment <- tragedy_words |> 
  inner_join(get_sentiments("bing"), by = join_by(word), relationship = "many-to-many")

# we can get a count of total positive and negative words in the four books
tragedy_sentiment_plot <- tragedy_sentiment |> 
  count(title, sentiment)

ggplot(tragedy_sentiment_plot, aes(x = sentiment, y = n, fill = title, alpha = sentiment)) +
  geom_col(position = position_dodge()) +
  scale_alpha_manual(values = c(0.5, 1)) +
  facet_wrap(vars(title)) +
  theme_bw()

# we can also divide each of the plays into groups of 100 lines, and then get the net sentiment of each group (number of positive words − number of negative words)
tragedies_split_into_lines <- tragedy_sentiment |> 
  # Divide lines into groups of 100
  mutate(line = row_number(),
         line_chunk = line %/% 100) |> 
  # Get a count of postiive and negative words in each 100-line chunk in each play
  count(title, line_chunk, sentiment) |> 
  # Convert the sentiment column into two columns named "positive" and "negative"
  pivot_wider(names_from = sentiment, values_from = n) |> 
  # Calculate net sentiment
  mutate(sentiment = positive - negative)

ggplot(tragedies_split_into_lines,
       aes(x = line_chunk, y = sentiment, fill = sentiment)) +
  geom_col() +
  scale_fill_viridis_c(option = "magma", end = 0.9) +
  facet_wrap(vars(title), scales = "free_x") +
  theme_bw()