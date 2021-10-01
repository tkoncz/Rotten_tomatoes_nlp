get_movies <- function(year) {

    ###
    #   This function is to get the top 100 movies html table into a data table
    #   filter for top 10 and create a column for the page with the reviews
    ###


    # Get URL components + final URL
    base <- 'https://www.rottentomatoes.com'
    subdomain <- 'top/bestofrt'
    query <- paste0('?year=',year)
    url <- paste0(c(base,subdomain,query), collapse = '/')

    # Get table with contents
    t<- read_html(url)

    Sys.sleep(10) # sleep for a while so that servers are not overloaded

    table <- t %>% html_table(fill = T)
    table <- as.data.table(table[[3]])
    table$link <- t %>%
        html_nodes('#main_container .col-full-xs') %>%
        html_nodes('.articleLink') %>%
        html_attr('href')

    # Filter only the top 10
    table <- table[Rank<=10,]

    # Add extra vars
    table$year <- year
    table$review_link <- paste0(c(base,table$link,'reviews'),collapse = '/')
    table[, review_link := paste0(base, '/', link,'/reviews')]

    return(table)

}


# Get reviews -------------------------------------------------------------

get_pages <- function(link) {

    t<- read_html(link)

    Sys.sleep(10)

    maxnum <- t %>%
        html_nodes('.pageInfo') %>%
        html_text() %>% .[1] %>%
        strsplit(split = ' ') %>% .[[1]] %>% .[4]


    pages <- paste0(link,'?type=&sort=&page=',1:maxnum)

    return(pages)

}

# TODO: Include years - this could easily be done in scraper, but I made a mistake by not adding it
# Extra hacks to get year back are available in the end of this script

get_reviews <- function(link) {

    print('Initiating script...')

    t<- read_html(link)

    print(paste0('HTML is read for ',link))

    wait = sample(c(5,6,7),1)
    Sys.sleep(wait)

    print('Sleeping is done')

    genre <- t %>% html_nodes('.center+ .bottom_divider li:nth-child(2)') %>% html_text()
    genre <- gsub("[\r\n ]", "", genre)


    movie <- t %>% html_nodes('.panel-bottom_divider .reviews-content') %>%
        html_nodes('.center') %>%
        html_nodes('.articleLink') %>%
        html_text()
    movie <- gsub("[\r\n ]", "", movie)

    print('Genre and movie name are successfully scraped')

    boxes <- t %>% html_nodes('.review_table_row')


    df <- lapply(boxes, function(x){
        t_list <- list()
        t_list[['name']] <- x %>% html_nodes('.articleLink') %>% html_text()

        t_list[['review']] <- x %>% html_nodes('.the_review') %>% html_text()

        t_list[['review_date']] <- x %>% html_nodes('.review-date') %>% html_text()

        t_list[['published']] <- x %>% html_nodes('.critic-publication') %>% html_text()

        review_link <- x %>%
            html_nodes('.review-link a') %>%
            html_attr('href')

        t_list[['review_link']] <- ifelse(identical(review_link,character(0)),'NULL', review_link)

        critic <- x %>%
            html_nodes('.critic-info') %>%
            html_nodes('.critic_name') %>%
            html_nodes('.small') %>%
            html_attr('class')

        t_list[['top_critic']] <- !identical(critic,character(0))

        return(data.frame(t_list))
    }) %>% rbindlist()



    df$genre <- genre
    df$movie <- movie

    print(paste0('Dataframe for ',link,' is ready'))

    return(df)
}
