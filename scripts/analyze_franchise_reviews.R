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
stop_words <- tidytext::stop_words %>%
    data.table() %>%
    .[lexicon == "onix"]

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

## TODO: drop James Bond (~ domain specific stopwords)

# n-grams
review_bigrams_by_movie <- reviews[, .(movie_id, short_review)] %>%
    unnest_tokens(output = bigram, input = short_review, token = "ngrams", n = 2)

review_bigrams_by_movie[, .N, bigram][order(-N)]

# removing bigrams of stopwords
stopwords <- stop_words[, word]

# show that regex should be as precise as possible. Example: "on" vs "bond"
grep(paste0(stopwords, collapse = "|"), "bond", value = TRUE)

# demo: "^" and "$"
test <- c("i am", "james", "ma'am")
grepl("am", test)
grepl("^am|am$", test)
grepl("^am|am$", test)
grepl("^am[[:space:]]|[[:space:]]am$", test)

stopwords_regex <- paste(
    paste(paste0("^", stopwords, "[[:space:]]"), collapse = "|"),
    paste(paste0("[[:space:]]", stopwords, "$"), collapse = "|"),
    collapse = "|"
)

review_bigrams_by_movie <- review_bigrams_by_movie %>%
    .[!grepl(stopwords_regex, bigram)]

review_bigrams_by_movie[, .N, bigram][order(-N)]

# sentiment analysis
# show different sentiment lexicons
tidytext::get_sentiments()

get_sentiments(lexicon = "afinn")
get_sentiments(lexicon = "bing")
get_sentiments(lexicon = "nrc")
get_sentiments(lexicon = "loughran")

sentiment_scores <- get_sentiments(lexicon = "afinn") %>% data.table()
sentiment_scores[, .N, keyby = "value"]

sentiment_scores[value == -5] %>% head(3)
sentiment_scores[value == 0] %>% head(3)
sentiment_scores[value == 5] %>% head(3)


review_sentiment_scores <- reviews[, .(short_review)] %>%
    unnest_tokens(output = word, input = short_review, drop = FALSE) %>%
    # be careful with merging! not all words have sentiment scores
    merge(sentiment_scores, by = "word") %>%
    .[, .(sentiment_score = sum(value)), by = c("short_review")]

review_sentiment_scores[order(-sentiment_score)] %>% head(3)
review_sentiment_scores[order(-sentiment_score)] %>% tail(3)

movies_w_sentiments <- reviews %>%
    # normally pls don't join on text fields...
    merge(review_sentiment_scores, by = "short_review") %>%
    .[,
        .(movie_sentiment_score = sum(sentiment_score)),
        by = c("movie_id", "media_score")
    ]


ggplot(
    movies_w_sentiments,
    aes(x = movie_sentiment_score, y = media_score)
) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        title = "Correlation between review sentiments and review scores",
        subtitle = "On the 27 movies in the James Bond franchise",
        caption = paste(
            "Data gathered from rottentomatoes.com",
            "Sentiment lexicon used: 'afinn'",
            sep = "\n"
        ),
        y = "Media Score on RT",
        x = "Sum of review word sentiment scores"
    ) +
    scale_y_continuous(limits = c(0, 125)) +
    theme_minimal()
