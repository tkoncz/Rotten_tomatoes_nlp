library(rvest)
library(data.table)
library(tidyverse)


# Get URLs ------------------------------------------------------------


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



# Actual scraping ---------------------------------------------------------

# first we take the top ten list of movies for 2018,2019,2020

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
