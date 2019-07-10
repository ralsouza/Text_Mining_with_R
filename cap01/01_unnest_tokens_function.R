
##### 1.2 The unnest_tokens function ####
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# Converter vetor em um data frame
library(dplyr)

text_df <- tibble(line = 1:4, text = text)

text_df

##### 
library(tidytext)
text_df %>% 
  unnest_tokens(word, text)

##### 1.3 Tidying the works of Jane Austen ####
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex('^chapter [\\divxlc]',
                                                 ignore_case = TRUE)))) %>% 
  ungroup()

original_books

##### Tokenizing Text
library(tidytext)

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

tidy_books

##### Remove Stopwords
data("stop_words")

tidy_books <- tidy_books %>% 
  anti_join(stop_words)

# Listar as palavras mais frequentes
tidy_books %>% 
  count(word, sort = TRUE)
