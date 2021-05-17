library(tidyverse)
library(lubridate)

df<- read_csv('../data/raw/rottentomatoes_raw.csv')

df<-
df %>% mutate(
    review = trimws(review),
    review_date = mdy(trimws(review_date))
    )



df<- 
    df %>% separate(genre, 
                    c('first_genre', 'second_genre'),
                    sep = ',', extra='merge', remove = F)

write_csv(df, '../data/clean/rottentomatoes_clean.csv')
