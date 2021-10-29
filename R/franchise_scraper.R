getMoviesForFranchise <- function(franchise_id) {
    franchise_base_url <- "https://www.rottentomatoes.com/franchise"
    franchise_url <- glue::glue("{franchise_base_url}/{franchise_id}")

    html <- read_html(franchise_url)

    movie_elements <- html %>%
        html_elements(xpath = "//ul[@class='franchise-media-list js-franchise-media-list']") %>%
        html_elements(xpath = "//li[@class='franchise-media-list__item']")

    movie_ids <- movie_elements %>%
        html_elements(xpath = "//div[@class='franchise-media-list__poster']//a")  %>%
        html_attr("href") %>%
        gsub("/m/", "", .) %>%
        trimws()

    movie_posters <- movie_elements %>%
        html_elements(xpath = "//div[@class='franchise-media-list__poster']//img")  %>%
        html_attr("src") %>%
        trimws()

    media_scores <- movie_elements %>%
        html_elements(xpath = "//div[@class='franchise-media-list__tomatometer']//strong") %>%
        html_text() %>%
        gsub("%", "", .) %>%
        trimws() %>%
        as.integer()

    audience_scores <- movie_elements %>%
        html_elements(xpath = "//div[@class='franchise-media-list__audiences']//strong") %>%
        html_text() %>%
        gsub("%", "", .) %>%
        trimws() %>%
        as.integer()

    movie_titles <- movie_elements %>%
        html_elements(xpath = "//h3[@class='franchise-media-list__h3']//a") %>%
        html_text() %>%
        trimws()

    movie_years <- movie_elements %>%
        html_elements(xpath = "//h3[@class='franchise-media-list__h3']//span") %>%
        html_text() %>%
        gsub("\\(|\\)", "", .) %>%
        trimws() %>%
        as.integer()

    movie_cast <- movie_elements %>%
        html_elements(xpath = "//div[@data-qa='franchise-media-cast']") %>%
        html_text() %>%
        gsub("Starring:", "", .) %>%
        trimws()


    all_same_length <- all(sapply(
        list(
            movie_ids, movie_posters, media_scores, audience_scores,
            movie_titles, movie_years, movie_cast
        ),
        FUN = function(x) identical(length(x), length(movie_elements))
    ))

    if (all_same_length) {
        return(data.table(
            movie_id = movie_ids,
            movie_title = movie_titles,
            movie_poster = movie_posters,
            media_score = media_scores,
            audience_score = audience_scores,
            movie_year = movie_years,
            movie_cast = movie_cast
        ))
    } else {
        message(glue::glue("Mismatch in attribute lengths for url: {url}"))
        return(NULL)
    }
}
