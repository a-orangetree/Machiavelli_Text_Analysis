library(tidyverse)

thePrince_raw <- as.tibble(read_lines('thePrince.txt'))

i_words <- c("i", "i've", "i'd", "i'm", "i'll")
u_words <- c("you", "you've", "you'd", "you'll")

thePrince <- thePrince_raw %>% 
  filter(value != "") %>%
  mutate(hasChapter = str_detect(value, 'CHAPTER'),
         chapterNumber = 0,
         isHeader = grepl("^[[:upper:]]+$", value),
         value = tolower(value),
         i_word_count = str_count(value, i_words),
         u_word_count = str_count(value, u_words),         
         document = row_number()) 

chapterNumber = 0
numberOfRows = dim(thePrince)[1]

for (row in seq(numberOfRows)) {
  
  if (thePrince$hasChapter[row] == TRUE) {
    chapterNumber = chapterNumber + 1
    thePrince$chapterNumber[row:numberOfRows] = chapterNumber
  }
}

thePrince <- thePrince %>% 
  mutate(chapterNumber = factor(chapterNumber))