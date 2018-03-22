extrafont::loadfonts(device="win")
extrafont::fonttable()
extrafont::font_import("C:/Windows/Fonts/", pattern = "Georgia")

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(tidytext)
library(quanteda)
library(ggplot2)
library(DT)

#ingest data
data <- read_csv("feature_winners_2007_2017.csv")

#adds fields for number of syllables, number of words, number of sentences,
#Flesch-Kincaid Reading Ease, and Flesch-Kincaid Reading Level.
#This is per graf data, so later I'll calculate it for the whole articles. 
data <- data %>%
  mutate(year = year(Date),
         syllables = nsyllable(Graf),
         sentences = nsentence(Graf),
         words = ntoken(Graf, remove_punct = TRUE),
         fk_ease = 206.835 - 1.105*(words/sentences) - 84.6*(syllables/words),
         fk_grade = 0.39*(words/sentences) + 11.8*(syllables/words) - 15.59) %>%
  arrange(Date)