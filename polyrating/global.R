library(shiny)
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)
library(wordcloud)

polyrating <- read_csv(
  "https://raw.githubusercontent.com/lemarpopal/PolyRatings-Shiny-App/master/vignettes/polyrating.csv"
) %>% 
  mutate(date = parse_date_time(date,"%m%y")) %>%
  drop_na() %>%
  slice(seq(0.3 * n()))

token_words <- read_csv("https://raw.githubusercontent.com/lemarpopal/PolyRatings-Shiny-App/master/vignettes/unique_poly.csv")

data(stop_words)

subjects <- polyrating %>%
  distinct(subject) %>%
  pull() %>%
  unlist()

review_words <- polyrating %>%
  unnest_tokens(word,review) %>%
  count(subject, word, sort = TRUE) %>%
  ungroup()

total_words <- review_words %>%
  group_by(subject) %>%
  summarize(total = sum(n))

review_words <- left_join(review_words, total_words)