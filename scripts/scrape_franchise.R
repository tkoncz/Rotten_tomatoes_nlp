source("global.R")

franchise_id <- "marvel_cinematic_universe"

franchise_elements <- getMoviesForFranchise(franchise_id)

movie_ids <- franchise_elements[grepl("/m/", rt_id), gsub("/m/", "", rt_id)]
short_reviews <- purrr::map(movie_ids, ~{
    Sys.sleep(1)
    getShortReviews(.x)
}) %>%
    rbindlist() %>%
    .[, rt_id := paste0("/m/", movie_id)]


franchise_movie_reviews <- merge(
    franchise_elements, short_reviews, by = "rt_id"
    # this is an inner join, will drop non-movies from `franchise_elements`
) %>%
    .[, -c("rt_id")]

fwrite(
    franchise_movie_reviews,
    glue("data/{franchise_id}_franchise_short_reviews.csv")
)
