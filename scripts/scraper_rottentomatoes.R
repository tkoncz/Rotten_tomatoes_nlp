source("global.R")

years <- c(2020)
all_movies <- purrr::map(years, ~{
    Sys.sleep(1)
    getMovies(.x)
}) %>%
    rbindlist()

movie_ids <- all_movies[, gsub("/m/", "", link)]
short_reviews <- purrr::map(movie_ids, ~{
    Sys.sleep(1)
    getShortReviews(.x)
}) %>%
    rbindlist()

# https://www.rottentomatoes.com/browse/dvd-streaming-all?minTomato=0&maxTomato=24&services=amazon;hbo_go;itunes;netflix_iw;vudu;amazon_prime;fandango_now&genres=1;2;4;5;6;8;9;10;11;13;18;14&sortBy=release
