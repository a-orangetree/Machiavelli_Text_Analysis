library(tidyverse)

# Import the raw text (for now, only the Prince)
thePrince_raw <- as.tibble(read_lines('thePrince.txt'))

# Create lists of "I words" and "You words"
i_words <- c(" i ", " i've ", " i'd ", " i'm ", " i'll", " me ", " my ")
u_words <- c(" you ", " you've ", " you'd ", " you're ", " you'll ")

# Create new features
thePrince <- thePrince_raw %>% 
  filter(value != "") %>%
  mutate(hasChapter = str_detect(value, 'CHAPTER'),
         chapterNumber = 0,
         isHeader = grepl("^[[:upper:]]+$", value),
         value = str_trim(str_to_lower(value)),
         num_words = lengths(gregexpr("\\W+", value)) + 1,
         characters = str_length(value),
         i_word_count = str_count(value, i_words),
         u_word_count = str_count(value, u_words),         
         document = row_number()) 

# Create variables needed for below iteration
chapterNumber = 0
numberOfRows = dim(thePrince)[1]

# Identify which rows belong to which chapter
for (row in seq(numberOfRows)) {
  
  if (thePrince$hasChapter[row] == TRUE) {
    chapterNumber = chapterNumber + 1
    thePrince$chapterNumber[row:numberOfRows] = chapterNumber
  }
}

# Recast chapter number as a factor
thePrince <- thePrince %>% 
  mutate(chapterNumber = factor(chapterNumber))