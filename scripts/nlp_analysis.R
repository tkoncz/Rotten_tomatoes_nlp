## Intro is in the markdown 

library(tidytext)
library(tidyverse)
library(igraph)
library(ggraph)
library(topicmodels)
library(widyr)
data(stop_words)

text_df<- read_csv('data/clean/rottentomatoes_clean.csv')

head(text_df)


# Summary stats


sum_df<- 
    text_df %>% 
    select(movie, first_genre, year, review ) %>% 
    group_by(movie,first_genre,year) %>% 
    count(movie) %>% rename(review_count =  n) %>% 
    left_join(text_df %>% 
                  unnest_tokens(word,review) %>% 
                  group_by(movie,first_genre,year) %>% 
                  count(movie) %>% 
                  rename(word_count = n)) %>% 
    mutate(avg_words_per_review = round(word_count/ review_count,2)) %>% 
    arrange(year,movie) %>% 
    mutate(word_count = word_count/1000) %>% 
    rename(Movie = movie, 
           Genre = first_genre, 
           Year = year, 
           'Reviews No.' = review_count, 'Word No. (in k)' = word_count, 'Word/Review' = avg_words_per_review)

sum(sum_df$`Reviews No.`)
sum(sum_df$`Word No. (in k)`)


# we first do tokenisation and remove stop words
tidy_reviews <- 
    text_df %>% 
    mutate(id = rownames(text_df)) %>% 
    unnest_tokens(word,review) %>% 
    anti_join(stop_words)



# Word frequency ----------------------------------------------------------


tidy_reviews %>%  
    count(word,sort = T) %>% 
    filter(n>400) %>% 
    mutate(word = reorder(word,n)) %>% 
    ggplot(aes(word,n)) + 
    geom_col(fill = 'navyblue') + 
    xlab('') + 
    coord_flip() +theme_bw()


tidy_reviews %>% 
    count(first_genre, word, sort = T) %>% arrange(desc(n)) %>% 
    group_by(first_genre) %>% slice(1:10) %>% 
    ggplot(aes(reorder_within(word, n, first_genre),n)) + 
    geom_col(fill = 'navyblue') + 
    xlab('') + scale_x_reordered() +
    coord_flip() +theme_bw() + 
    facet_wrap(~first_genre, scales = 'free')
   

## Let's see word frequency differences between top critic and critic

frequency <- 
tidy_reviews %>% 
    count(top_critic, word) %>% 
    group_by(top_critic) %>% 
    mutate(proportion = n/sum(n)) %>% select(-c(n)) %>% 
    mutate( top_critic = ifelse(top_critic,'Top_critic','Critic')) %>%  
    pivot_wider(names_from = top_critic, values_from = proportion) 


ggplot(frequency,aes(x=Critic, y = Top_critic))+ # geom_point( color = 'navyblue', size = 3) + 
    geom_text(aes(label = word), hjust = 0, size = 3) + 
    theme_bw() 

#Correlation is massive 
cor.test(data =frequency, ~Critic + Top_critic)



# Sentiment analysis ------------------------------------------------------

get_sentiments('nrc')
get_sentiments('bing')
get_sentiments('afinn')

# Comparison of net sentiments for the three lexicons for each review 

afinn <- 
tidy_reviews %>% 
    inner_join(get_sentiments('afinn')) %>% 
    group_by(index = id) %>% 
    summarise(sentiment = sum(value)) %>% mutate(method = 'AFINN')

bing_and_nrc <- 
bind_rows (
    tidy_reviews %>% 
        inner_join(get_sentiments('bing')) %>% 
        mutate(method = 'Bing'),
    
    tidy_reviews %>% 
        inner_join(get_sentiments('nrc') %>% 
                       filter(sentiment %in% c('positive','negative'))) %>% 
        mutate(method = 'NRC')

) %>% 
    count(method, index = id, sentiment) %>% 
    spread(sentiment,n,fill = 0) %>% 
    mutate(sentiment = positive-negative)

bind_rows(afinn,
          bing_and_nrc) %>% 
    ggplot(aes(sentiment, fill = method)) + scale_fill_brewer('Dark2') +
    geom_density(bw=0.5 )+facet_wrap(~method, scales = 'free') +
    theme_bw() + xlab('Net sentiment') 

# let's go with Afinn and see how review sentiments change over time 

ts_reviews<-
tidy_reviews %>% 
    inner_join(get_sentiments('afinn')) %>% 
    group_by(index = id, movie, review_date) %>% 
    summarise(sentiment = sum(value)) %>% arrange(movie,review_date) %>% as.data.frame() %>% 
    mutate(chg = movie !=dplyr::lag(movie,1),
           chg = ifelse(is.na(chg),TRUE,chg))  %>% 
    group_by(grp = cumsum(chg == T)) %>% 
    mutate(counter = row_number(),
           cumsum = ave(sentiment, cumsum(chg == T), FUN = cumsum)) 

ts_reviews %>% 
    ggplot(aes(x = counter, y = sentiment, color = movie)) + 
    geom_line() +theme_bw() + 
    theme(legend.position = 'none') + xlab('')


ts_reviews %>% 
    ggplot(aes(x = counter, y = cumsum, color = movie)) + 
    geom_line() +theme_bw() + 
    theme(legend.position = 'none') + xlab('')

# We now do tf-idf - frequency of a term compared to how rarely it is used
# idf - how many information the word provides 


tidy_reviews %>% count(first_genre, word, sort = T) %>% 
    ungroup() %>%
    bind_tf_idf(word,first_genre,n) %>% 
    group_by(first_genre) %>% top_n(10,tf_idf) %>%  
    ggplot(aes(reorder_within(word, n, first_genre),n))  + 
    geom_col(show.legend = F, fill = 'navyblue') + 
    facet_wrap(~first_genre, scales = 'free') +
    scale_x_reordered() +
    coord_flip() + 
    theme_bw() + 
    xlab('')


# we will look for bigrams now 

bigram_reviews <- 
    text_df %>% mutate(id = rownames(text_df)) %>% unnest_tokens(bigram,review, token = 'ngrams', n = 2)

# let's filter out stop words 

bigram_counts <- bigram_reviews %>% 
    separate(bigram,c('word1', 'word2'), sep=  ' ') %>% 
    filter(
        !word1 %in% stop_words$word ,
        !word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = T )

# combine bigrams with tf-idf

bigram_reviews %>% count(first_genre,bigram, sort =T) %>% 
    bind_tf_idf(bigram, first_genre, n) %>% 
    arrange(desc(tf_idf)) %>% 
    separate(bigram,c('word1', 'word2'), sep=  ' ') %>% 
    filter(
        !word1 %in% stop_words$word ,
        !word2 %in% stop_words$word) %>% 
    unite(col = 'bigram',word1:word2, sep =' ' ) %>% 
    group_by(first_genre) %>% 
    top_n(10,tf_idf) %>% 
    ggplot(aes(reorder_within(bigram, n, first_genre),n))  + 
    geom_col(show.legend = F, fill = 'navyblue') + 
    facet_wrap(~first_genre, scales = 'free') +
    scale_x_reordered() +
    coord_flip() + 
    theme_bw() + 
    xlab('')

# Maybe it is worth looking at not + word for bigrams
not_words <- 
bigram_reviews %>% count(first_genre,bigram, sort =T) %>% 
    bind_tf_idf(bigram, first_genre, n) %>% 
    arrange(desc(tf_idf)) %>% 
    separate(bigram,c('word1', 'word2'), sep=  ' ') %>% 
    filter(word1 == 'not') %>% inner_join(get_sentiments('afinn'), by= c(word2 = 'word') )

# they were very rare , it doesn't really make sense to control for them


bigram_graph <- 
bigram_counts %>% select(from = word1, to= word2, n) %>% filter(n>30) %>% graph_from_data_frame()

a<- grid::arrow(type = 'closed', length = unit(.15, 'inches'))


ggraph(bigram_graph, layout = 'fr') +
    geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow= a, end_cap = circle(.07,'inches'))+
    geom_node_point(color = 'lightblue', size = 5)+ geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
    theme_void()

# score can also be expressed as  (positive-negative) / (positive + negative)


#This is to see pairwise correlations between genres

genre_cors <- tidy_reviews %>% 
    count(first_genre, word, sort= T) %>% 
    pairwise_cor(first_genre, word,n, sort = T) %>% tibble()

View(genre_cors)

genre_cors %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = correlation, width = correlation))+
    geom_node_point(color = 'lightblue', size = 5)+ 
    geom_node_label(aes(label = name), repel = T)+
    theme_void()


# moview cors

movie_cors <- tidy_reviews %>% 
    count(movie, word, sort= T) %>% 
    pairwise_cor(movie, word,n, sort = T) %>% tibble()

View(movie_cors)

movie_cors %>% 
    filter(correlation >.33) %>% 
    graph_from_data_frame() %>% 
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = correlation, width = correlation))+
    geom_node_point(color = 'lightblue', size = 5)+ 
    geom_node_label(aes(label = name), repel = T)+
    theme_void()

# Topic modelling  --------------------------------------------------------

# We can create two topics first 

# We can check the differences in beta 

# Per document per topic probability -> gamma // if gamma for given document of given topic is 20%,
# It means there is a 20% chance that this document was generated from given topic

colnames(tidy_reviews)

reviews_dtm <- 
tidy_reviews %>% 
    unite(document, first_genre, id) %>% 
    count(document,word) %>% cast_dtm(document,word,n)

reviews_lda<- LDA(reviews_dtm, k = 8, control = list(seed = 20210514))

reviews_lda %>% tidy() %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup() %>% 
    mutate(term = reorder(term,beta)) %>% 
    ggplot(aes(term,beta,fill = factor(topic)))+geom_col(show.legend = F) + 
    facet_wrap(~topic, scales = 'free') + 
    coord_flip()

reviews_lda %>% 
    tidy(matrix = 'gamma') %>% 
    separate(document, c('genre', 'id'), sep = '_') %>% 
    mutate(top_critic = reorder(genre, gamma * topic)) %>% ggplot(aes(factor(topic), gamma)) + 
    geom_boxplot() + facet_wrap(~genre, scales = 'free')


# most negative or most positive message 

# not words to negate words , where negate_words <- c('not', 'without', 'no',"can't", "don't", "won't")