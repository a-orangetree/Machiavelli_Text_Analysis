source('createCorpus.R')

chapter_summary <- thePrince %>% 
  group_by(chapterNumber) %>% 
  summarise(total_words = sum(num_words),
            total_i_words = sum(i_word_count),
            perc_i_words = total_i_words / total_words,
            total_u_words = sum(u_word_count),
            perc_u_words = total_u_words / total_words
            ,characters_per_word = sum(characters) / total_words)

# TODO: add in sentiment averages per chapter to the 'chapter_summary' object

##################
# Visualize sentiment by chapter

###### The below display COUNTS of emotion per chapter. These values need to 
###### be standardized, as a chapter with more words is more likely to have more
###### words indicating emotion.

# NRC
nrc_by_chapter <- words_in_chapters %>% 
  filter(!is.na(nrc)) %>% 
  select(-term, -bing, -afinn) %>% 
  group_by(chapterNumber) %>% 
  count(nrc)

ggplot(nrc_by_chapter, aes(chapterNumber, n)) +
  geom_point() +
  facet_wrap(~ nrc, scales = 'free')

# BING
bing_by_chapter <- words_in_chapters %>% 
  filter(!is.na(bing)) %>% 
  select(-term, -nrc, -afinn) %>% 
  group_by(chapterNumber) %>% 
  count(bing)

ggplot(bing_by_chapter, aes(chapterNumber, n)) +
  geom_point() +
  facet_wrap(~ bing, scales = 'free')

# AFINN
afinn_by_chapter <- words_in_chapters %>% 
  filter(!is.na(afinn)) %>% 
  select(-term, -bing, -nrc) %>% 
  group_by(chapterNumber) %>% 
  count(afinn)

ggplot(afinn_by_chapter, aes(chapterNumber, n)) +
  geom_point() +
  facet_wrap(~ afinn, scales = 'free')