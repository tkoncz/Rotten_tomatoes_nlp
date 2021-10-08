getMovies <- function(year) {

    ###
    #   This function is to get the top 100 movies html table into a data table
    #   and create a column for the page with the reviews
    ###


    # Get URL components + final URL
    base_url <- 'https://www.rottentomatoes.com'
    subdomain <- 'top/bestofrt'
    query <- paste0('?year=', year)
    url <- paste(base_url, subdomain, query, sep = '/')

    # Get table with contents
    t <- read_html(url)

    table <- t %>% html_table(fill = T)
    table <- as.data.table(table[[3]])
    table$link <- t %>%
        html_nodes('#main_container .col-full-xs') %>%
        html_nodes('.articleLink') %>%
        html_attr('href')

    # Add extra vars
    table[, year := year]
    table[, review_link := paste0(c(base_url, link, 'reviews'), collapse = '/')]
    table[, review_link := paste0(base_url, '/', link, '/reviews')]

    return(table)
}


getShortReviews <- function(movie_id) {
    url <- glue::glue("https://www.rottentomatoes.com/m/{movie_id}/reviews")
    tryCatch(
        {html <- read_html(url)},
        error = function(cond) {
            message(glue::glue("URL does not seem to exist: {url}"))
            message("Here's the original error message:")
            message(cond)
            return(NULL)
        }
    )

    if(length(html_element(html, xpath = '//div[@class="reviews-movie"]')) == 0) {
        message(glue::glue("No reviews found for url: {url}"))
        return(NULL)
    }

    reviews <- html %>%
        html_element(xpath = '//div[@class="reviews-movie"]') %>%
        html_element(xpath = '//div[@class="review_table"]') %>%
        html_elements(xpath = '//div[@class="row review_table_row"]')


    critic_names <- reviews %>%
        html_elements(xpath = '//div[@class="col-xs-8 critic-info"]') %>%
        html_elements(xpath = 'div[@class="col-sm-17 col-xs-32 critic_name"]') %>%
        html_elements(xpath = 'a[@class="unstyled bold articleLink"]') %>%
        html_text() %>%
        trimws()

    review_dates <- reviews %>%
        html_elements(xpath = '//div[@class="review_area"]') %>%
        html_elements(xpath = '//div[@class="review-date subtle small"]') %>%
        html_text() %>%
        trimws()

    short_reviews <- reviews %>%
        html_elements(xpath = '//div[@class="review_area"]') %>%
        html_elements(xpath = '//div[@class="review_desc"]') %>%
        html_elements(xpath = '//div[@class="the_review"]') %>%
        html_text() %>%
        trimws()

    # full_review_links <- reviews %>%
    #     html_elements(xpath = '//div[@class="review_area"]') %>%
    #     html_elements(xpath = '//div[@class="small subtle review-link"]/a') %>%
    #     html_attr("href") %>%
    #     trimws()

    all_same_length <- all(sapply(
        # list(critic_names, review_dates, short_reviews, full_review_links),
        list(critic_names, review_dates, short_reviews),
        FUN = function(x) identical(length(x), length(reviews))
    ))

    if (all_same_length) {
        return(data.table(
            movide_id = movie_id,
            reviews_page = url,
            critic_name = critic_names,
            review_date = review_dates,
            short_review = short_reviews
            # , full_review_link = full_review_links
        ))
    } else {
        message(glue::glue("Mismatch in attribute lengths for url: {url}"))
        return(NULL)
    }
}
