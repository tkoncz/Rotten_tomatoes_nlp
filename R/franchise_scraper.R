getMoviesForFranchise <- function(franchise_id) {
    franchise_base_url <- "https://www.rottentomatoes.com/franchise"
    franchise_url <- glue::glue("{franchise_base_url}/{franchise_id}")

    html <- read_html(franchise_url)

    franchise_elements <- html %>%
        html_elements(xpath = "//ul[@class='franchise-media-list js-franchise-media-list']") %>%
        # `and .//div[@class='franchise-media-list__tomatometer']`: only scrape items that have a score
        html_elements(xpath = "./li[@class='franchise-media-list__item' and .//div[@class='franchise-media-list__tomatometer']]")

    content_type <- franchise_elements %>%
        html_attr("data-franchise-type")

    ids <- franchise_elements %>%
        html_elements(xpath = "./div[@class='franchise-media-list__poster']//a") %>%
        html_attr("href") %>%
        trimws()

    posters <- franchise_elements %>%
        html_elements(xpath = "./div[@class='franchise-media-list__poster']/a/img")  %>%
        html_attr("src") %>%
        trimws()

    media_scores <- franchise_elements %>%
        html_elements(xpath = "./div/div[@class='franchise-media-list__score']/div[@class='franchise-media-list__tomatometer']//strong") %>%
        html_text() %>%
        gsub("%", "", .) %>%
        trimws() %>%
        as.integer()

    # audience_scores <- franchise_elements %>%
    #     html_elements(xpath = "./div/div[@class='franchise-media-list__score']/div[@class='franchise-media-list__audiences']//strong") %>%
    #     html_text() %>%
    #     gsub("%", "", .) %>%
    #     trimws() %>%
    #     as.integer()

    titles <- franchise_elements %>%
        html_elements(xpath = "./div/h3[@class='franchise-media-list__h3']//a") %>%
        html_text() %>%
        trimws()

    release_years <- franchise_elements %>%
        html_elements(xpath = "./div/h3[@class='franchise-media-list__h3']//span") %>%
        html_text() %>%
        gsub("\\(|\\)", "", .) %>%
        trimws() %>%
        as.integer()

    casts <- franchise_elements %>%
        html_elements(xpath = "./div/div[@data-qa='franchise-media-cast']") %>%
        html_text() %>%
        gsub("Starring:", "", .) %>%
        trimws()

    all_same_length <- all(sapply(
        list(
            ids, posters, media_scores,
            # audience_scores,
            titles, release_years, casts
        ),
        FUN = function(x) identical(length(x), length(franchise_elements))
    ))

    if (all_same_length) {
        return(data.table(
            rt_id = ids,
            title = titles,
            poster = posters,
            media_score = media_scores,
            # audience_score = audience_scores,
            release_year = release_years,
            cast = casts
        ))
    } else {
        message(glue::glue(
            "
                Mismatch in attribute lengths for url: {franchise_url},
                franchise_elements: {length(franchise_elements)},
                content_type: {length(content_type)},
                ids: {length(ids)},
                posters: {length(posters)},
                media_scores: {length(media_scores)},
                titles: {length(titles)},
                release_years: {length(release_years)},
                casts: {length(casts)}
            "
         ))

        return(NULL)
    }
}
