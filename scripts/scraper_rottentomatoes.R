source("global.R")

years <- c(2018,2019,2020)
all_movies <- rbindlist(lapply(years,get_movies))

# second we get the links where the reviews lie and save this list to disk

review_pages <- lapply(all_movies$review_link,get_pages) %>% unlist()
saveRDS(review_pages,'../data/raw/review_pages.rds')

# third we scrape the reviews 
final_df<- rbindlist(lapply(review_pages, get_reviews))

# write all to disk
write_csv(final_df,'../data/raw/rottentomatoes_raw.csv')


# Addition: Adding years to final df --------------------------------------

## I made an error not to incorporate years into my final scraper
# I added it back this way as it would take too much to rerun the scraping
# This is not an optimal solution, 

all_movies <- all_movies[,c('year','Title')]
all_movies[, title :=  str_sub(Title,end=-8)]
all_movies[, title := gsub("[\r\n ]", "", title)]

final_df<- fread('../data/raw/rottentomatoes_raw.csv')

final_df<- merge(final_df, all_movies[,c('year','title')], 
                  all.x = T, by.x = 'movie', by.y = 'title')

# I had three unsuccessfull joins
missing_movies <- unique(final_df[is.na(year),movie])

# Mission impossible is 2018, Parasite is 2019, Portrait of a lady on fire is 2020
final_df[movie == missing_movies[1], year := 2018 ] 
final_df[movie == missing_movies[2], year := 2019 ] 
final_df[movie == missing_movies[3], year := 2020 ] 

#checking if everything worked
colSums(is.na(final_df))
final_df[,.(moviecount = length(unique(movie))),by=year]


write_csv(final_df,'../data/raw/rottentomatoes_raw.csv')
