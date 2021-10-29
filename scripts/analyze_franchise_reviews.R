source("global.R")

franchise_id <- "james_bond_007"
reviews <- fread(glue("data/{franchise_id}_franchise_short_reviews.csv"))


# tokenize
review_words_by_movie <- reviews[, .(movie_id, short_review)] %>%
    unnest_tokens(output = word, input = short_review)

review_words_by_movie[, .N, movie_id][order(-N)]

# most frequent words
review_words_by_movie[, .N, word][order(-N)]

# stopwords
stop_words <- tidytext::stop_words %>% data.table()

review_words_by_movie <- review_words_by_movie[!stop_words, on = "word"]

# most frequent words
word_frequencies <- review_words_by_movie %>%
    .[, .(num_occurance = .N), by = "word"] %>%
    .[num_occurance > 5] %>%
    .[order(-num_occurance)]

word_frequencies

wordcloud::wordcloud(
    words = word_frequencies[, word],
    freq = word_frequencies[, num_occurance]
)
