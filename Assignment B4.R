suppressPackageStartupMessages(library(janeaustenr))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stopwords))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(datateachr))

# Option 1
book_df <- tibble(txt = emma)
stop_words <- data.frame(stopwords::stopwords("en", source = "snowball"))
colnames(stop_words) <- "token"
colnames(book_df) <- "col"
book_df <- book_df %>%
  unnest_tokens(token, col) %>%
  anti_join(stop_words,by = "token")
  

frequency_dataframe = book_df %>% count(token) %>% filter(n>200) %>% mutate(token = reorder(token, n))
head_df <- head(frequency_dataframe, 15) 
ggplot(head_df, aes(x = token, y = n)) + geom_col(fill = "pink", alpha = 1.4) + coord_flip() + labs(x = "Word \n", y = "\n Count ", title = "Most common words In Emma \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "grey", fontface = "bold")

# Option 3
## Creating list-column of model objects
head(ChickWeight)
list_column <- ChickWeight %>%
  filter(Chick == 1 | Chick == 25 | Chick == 35 | Chick == 45) %>%
  group_by(Diet) %>%
  nest()

list_model <- list_column %>%
  mutate(model = map(data, ~lm(weight ~ Time, data = .x)))

## Evaluate each model
list_coeff <- list_model %>%
  mutate(coeff = map(model, tidy))
print(list_coeff)

## Unnest the resulting calculations
list_unnest <- list_coeff %>%
  unnest(coeff)
print(list_unnest)

## Produce plot for each linear model
ChickWeight %>%
  filter(Chick == 1 | Chick == 25 | Chick == 35 | Chick == 45) %>%
  ggplot(aes(x=Time, y=weight, colour = Diet)) +
  geom_smooth(method = lm, se = FALSE, fullrange=TRUE, aes(color = Diet))