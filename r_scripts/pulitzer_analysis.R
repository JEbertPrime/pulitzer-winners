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
data$graf <- sapply(data$graf,function(row) iconv(row, "latin1", "ASCII", sub=""))
n
data$lower_graf <- tolower(data$graf)
#adds fields for number of syllables, number of words, number of sentences,
#Flesch-Kincaid Reading Ease, and Flesch-Kincaid Reading Level.
#This is per graf data, so later I'll calculate it for the whole articles. 
data <- data %>%
  mutate(year = year(date),
         syllables = nsyllable(lower_graf),
         sentences = nsentence(graf),
         words = ntoken(graf, remove_punct = TRUE),
         fk_ease = 206.835 - 1.105*(words/sentences) - 84.6*(syllables/words),
         fk_grade = 0.39*(words/sentences) + 11.8*(syllables/words) - 15.59) %>%
  arrange(date)
summary_data <- data.frame(data$headline, data$syllables, data$sentences, data$words)
summary_data <- aggregate(summary_data[2:4], list(summary_data$data.headline), FUN=sum)
summary_data <- summary_data %>%
  mutate(fk_ease = 206.835 - 1.105*(data.words/data.sentences) - 84.6*(data.syllables/data.words),fk_grade = 0.39*(data.words/data.sentences) + 11.8*(data.syllables/data.words) - 15.59) 
ggplot(data, aes(x=reorder(headline, date), y=fk_grade, fill=headline), color=NA) +
  geom_boxplot(color="#909090") +
  ylab("Reading Level") +
  xlab("") +
  ggtitle("Pulitzer Winning Feature Reading Grades", subtitle="Winners from 2007-2017") +
  coord_flip() +
  scale_y_continuous(limits=c(-5,20), breaks = c(0,5,10,15,20)) +
  theme_minimal(base_size=25, base_family="Garamond") +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        axis.ticks.x=element_blank(),
        legend.text = element_text(color="#909090", size = 10))
ggsave("box_plot.png",width=18, height=7, units = "in", dpi = 300)
ggplot(data, aes(x=graf_id, y=fk_grade, color=headline)) +
  geom_point(aes(color=headline, fill=headline), alpha=.8) +
  ylab("Reading Level") +
  xlab("") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
