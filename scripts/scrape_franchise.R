source("global.R")

franchise_id <- "james_bond_007"

franchise_movies <- getMoviesForFranchise(franchise_id)

movie_ids <- franchise_movies[, movie_id]
short_reviews <- purrr::map(movie_ids, ~{
    Sys.sleep(1)
    getShortReviews(.x)
}) %>%
    rbindlist()


franchise_movie_reviews <- merge(
    franchise_movies, short_reviews, by = "movie_id"
)

fwrite(
    franchise_movie_reviews,
    glue("data/{franchise_id}_franchise_short_reviews.csv")
)
