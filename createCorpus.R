library(tm)
library(tidytext)
source('importAndClean.R')

textCorpus <- Corpus(VectorSource(thePrince$value))
textCorpus <- tm_map(textCorpus, removePunctuation)
textCorpus <- tm_map(textCorpus, removeNumbers)
textCorpus <- tm_map(textCorpus, content_transformer(tolower))
textCorpus <- tm_map(textCorpus, stripWhitespace)
textCorpus <- tm_map(textCorpus, stemDocument)

# textCorpus <- tm_map(textCorpus, removeWords, stopwords('english'))
myStopwords <- c('can', 'the', 'and', 'that', 'not', 'his', 'have', 'for', 'they', 'him',
                 'who', 'with', 'them', 'are', 'this', 'but', 'was', 'their', 'from', 'other',
                 'which', 'those', 'had', 'becaus', 'would', 'when', 'been', 'all', 'were',
                 'there', 'has', 'than', 'may', 'such', 'these', 'therefor', 'ought', 'thing',
                 'cannot', 'how', 'nor', 'did', 'himself', 'should', 'either', 'much', 
                 'without', 'themselv', 'well', 'whom', 'what', 'seen', 'find', 'after',
                 'although', 'both', 'then')
textCorpus <- tm_map(textCorpus, removeWords, myStopwords)


##############
dtm <- DocumentTermMatrix(textCorpus)
dtm2 <- removeSparseTerms(dtm, 0.99)
dtm3 <- tidy(dtm2) %>% mutate(document = as.integer(document))

words_in_chapters <- left_join(dtm3
                               , select(thePrince, chapterNumber, document)
                               , by = c('document')) %>% 
  select(-document, -count)

most_common_words <- count(words_in_chapters, term) %>% 
    arrange(desc(n))


########### Import Sentiment Dictionaries ################
afinn_dict <- get_sentiments("afinn") %>% rename('afinn' = 'score')

bing_dict <- get_sentiments("bing") %>% 
  rename('bing' = 'sentiment') %>% 
  mutate(bing = ifelse(bing == 'negative', 0, ifelse(bing == 'positive', 1, NA))
         ,bing = as.integer(bing))

nrc_dict <- get_sentiments("nrc") %>% rename('nrc' = 'sentiment')


########### Add Sentiment Dictionaries to main data structure
words_in_chapters <- left_join(words_in_chapters, bing_dict, by = c('term' = 'word')) 
words_in_chapters <- left_join(words_in_chapters, afinn_dict, by = c('term' = 'word')) 
words_in_chapters <- left_join(words_in_chapters, nrc_dict, by = c('term' = 'word')) 

###########
nrc_by_chapter <- words_in_chapters %>% 
  filter(!is.na(nrc)) %>% 
  select(-term, -bing, -afinn) %>% 
  group_by(chapterNumber) %>% 
  count(nrc)

bing_by_chapter <- words_in_chapters %>% 
  filter(!is.na(nrc)) %>% 
  select(-term, -nrc, -afinn) %>% 
  group_by(chapterNumber) %>% 
  count(bing)

afinn_by_chapter <- words_in_chapters %>% 
  filter(!is.na(nrc)) %>% 
  select(-term, -bing, -nrc) %>% 
  group_by(chapterNumber) %>% 
  count(afinn)

ggplot(bing_by_chapter, aes(chapterNumber, n)) +
  geom_point() +
  facet_wrap(~ bing, scales = 'free')

