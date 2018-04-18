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
library(ggthemes)

#ingest data
data <- read_csv("feature_winners_2007_2017.csv")
#Necessary to avoid encoding errors when converting to lowercase
data$graf <- sapply(data$graf,function(row) iconv(row, "latin1", "ASCII", sub=""))
full_articles <- aggregate(graf ~ headline + date, data=data, paste) 
article_text<- data.frame(graf = unlist(full_articles$graf))
write.table(full_articles, "full_articles.txt")
#lowercase is needed for nsyllable, which bugs out when encountering ALL CAPS
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
#create new data frame with summed values of syllable count, word count, and sentence count
summary_data <- data.frame(data$headline, data$syllables, data$sentences, data$words, data$year_won)
summary_data <- aggregate(summary_data[2:4], list(summary_data$data.headline), FUN=sum)
summary_data <- summary_data %>%
  mutate(fk_ease = 206.835 - 1.105*(data.words/data.sentences) - 84.6*(data.syllables/data.words),
         fk_grade = 0.39*(data.words/data.sentences) + 11.8*(data.syllables/data.words) - 15.59) 
names(summary_data)[names(summary_data) == 'Group.1'] <- 'headline'
summary_data <- merge(x=summary_data, data[ ,c('headline','date')], by='headline', all.x=TRUE)
summary_data <- unique(summary_data)
#Makes a cute box plot
ggplot(summary_data, aes(x=reorder(headline, date), y=fk_grade), color=NA) +
  geom_bar(stat='identity',color="#909090", fill="blue") +
  ylab("Reading Level") +
  xlab("") +
  ggtitle("Pulitzer Winning Feature Reading Grades", subtitle="Winners from 2007-2017") +
  coord_flip() +
  scale_y_continuous(limits=c(0,15), breaks = c(0,5,10,15,20)) +
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

# load lexicon from https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
bing <- get_sentiments("bing")

# sentiment by paragraph
sentiments <- data %>%
  unnest_tokens(word, graf) %>%
  filter(str_detect(word, "[a-z]")) %>%
  # match to lexicon
  inner_join(bing, by = "word")

sentiments_counts <- sentiments %>%
  group_by(headline, date, author) %>%
  count(sentiment) %>%
  arrange(-n)

positive_freqs <- sentiments_counts %>%
  left_join(sentiments_counts %>% 
              group_by(headline,date,author) %>% 
              summarise(total = sum(n))) %>%
  mutate(percent = round(n/total*100,2)) %>%
  filter(sentiment == "positive")

# sentiment chart
ggplot(positive_freqs, aes(x=date, y=percent, color=author, size=5)) +
  geom_point(alpha=0.5) +
  geom_smooth(se=F, color="black", method="lm", size=0.5, linetype = "dotted") +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_y_continuous(limits = c(20,90)) +
  xlab("") +
  ylab("% positive words") +
  guides(col = guide_legend(ncol = 2, override.aes = list(size = 4))) +
  theme_wsj() +
  theme(
        legend.text = element_text(color="#909090", size = 18),
        panel.grid.minor = element_blank())
# break text into bigrams
bigrams <- data %>% 
  unnest_tokens(bigram,graf, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  # remove stop words from tidytext package 
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  filter(str_detect(first, "[a-z]"),
         str_detect(second, "[a-z]")) %>%
  group_by(headline,author,date) %>%
  count(bigram) %>%
  arrange(-n)

bigram_freqs <- bigrams %>% 
  left_join(bigrams %>% 
              group_by(headline,author,date) %>% 
              summarise(total = sum(n))) %>%
  mutate(percent = n/total*100) %>%
  group_by(headline,author,date)

# get the top bigram for each address
top_bigrams <- bigram_freqs %>%
  top_n(1) %>%
  arrange(-percent)

top_bigram_freqs <- bigram_freqs %>%
  semi_join(top_bigrams) %>%
  ungroup() %>%
  arrange(-percent) %>%
  mutate(year = year(date),
         address = paste0(headline,", ",year))

# some cleaning for display in chart
top_bigram_freqs$address <- gsub("William J.","Bill", top_bigram_freqs$address)

# color palette for this chart
bigram_pal <- c("#1482EE","#228B22","#686868","#FF3300","#EEC900")

# chart
ggplot(top_bigram_freqs[1:12,], aes(x=reorder(address,percent), y=percent, label=bigram)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(y = 1.5), color = "#FFFFFF", size = 7) +
  theme_minimal(base_size = 24, base_family = "ProximaNova-Semibold") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  xlab("") +
  ylab("% of word pairs used") +
  coord_flip()

##############################################################################################

data <- data %>%
  arrange(date)

# empty data frames 
first_words = data_frame()
old_words <- data_frame()

# loop through each address, comparing to predecessors to select new words
n <- 1
for (l in full_articles$headline) {
  previous <- data[n-1,]
  previous_words <- previous %>%
    unnest_tokens(word, graf) %>%
    filter(str_detect(word, "[a-z]")) %>%
    anti_join(stop_words) %>%
    unique()
  old_words <- bind_rows(old_words, previous_words)
  tmp <- data %>%
    filter(headline == l)
  tmp_words <- tmp %>%
    unnest_tokens(word, graf) %>%
    filter(str_detect(word, "[a-z]")) %>%
    anti_join(stop_words) %>%
    unique()
  new_words = base::setdiff(tmp_words$word, old_words$word)
  tmp_df <- data_frame(headline = l, word = new_words)
  first_words <- bind_rows(first_words,tmp_df)
  n <- n+1
}

first_words <- inner_join(data,first_words) %>%
  select(headline,author,date,word) %>%
  arrange(desc(date))

datatable(first_words)
