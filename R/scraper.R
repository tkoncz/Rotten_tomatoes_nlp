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


getShortReviews <- function(url) {
    html <- read_html(url)

    critics <- html %>%
        html_elements(xpath = '//*[@class="row review_table_row"]')

    critic_names <- critics %>%
        html_elements(xpath = '//*[@class="col-sm-17 col-xs-32 critic_name"]/a') %>%
        html_text() %>%
        trimws()

    short_reviews <- critics %>%
        html_elements(xpath = '//*[@class="the_review"]') %>%
        html_text() %>%
        trimws()

    data.table(
        review_link = url,
        critic_name = critic_names,
        short_review = short_reviews,
        full_review_link = full_review_links
    )
}
