#####################################################################################
### Project : Amazon Reviews
### Script : Text_mining_amazon_reviews.R
### Description : Scraping Amazon reviews and analyzing text
#####################################################################################

#####################################################################################
### Setting up Environment
#####################################################################################

# Load libraries
pkgs <- c("rvest", "dplyr", "tm", "SnowballC", "ggplot2", "wordcloud")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Load user defined functions
#####################################################################################

# Function for scraping product reviews from Amazon
# Parameters: product and product_id from the Amazon url:
#   e.g. for https://www.amazon.com/Samsung-Factory-Unlocked-International-Platinum/dp/B01CJU9MJI/
#        product = Samsung-Factory-Unlocked-International-Platinum,
#        product_id = B01CJU9MJI
# Return: dataframe of review stars, title, author, date, and text
amazon_scraping <- function(product, product_id) {
  # Create url to access product reviews
  website     <- "https://www.amazon.com"
  properties  <- "ref=cm_cr_arp_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber="
  product_url <- paste(website, product, "product-reviews", product_id, properties, sep = "/")
  
  # Fetch html data for page 1 of product reviews
  page      <- 1
  page_url  <- paste0(product_url, page)
  html_data <- read_html(page_url)
  
  # Find total number of pages of reviews
  pages       <- html_data %>% html_nodes(".a-pagination a") %>% html_text()
  total_pages <- as.numeric(pages[length(pages) - 1])
  
  # Iterate through each page and extract reviews
  reviews <- data.frame()
  for (page in 1:total_pages) {
    # Print current page to the console
    print(paste(page, "of", total_pages))
    
    # Fetch html data for current page
    page_url  <- paste0(product_url, page)
    html_data <- read_html(page_url)
    
    # Extract reviews for current page
    review <- data.frame(stars  = (html_data %>% html_nodes(".review-rating") %>% html_text())[-1:-2],
                         title  = (html_data %>% html_nodes(".review-title")  %>% html_text())[-1:-2],
                         author =  html_data %>% html_nodes(".author")        %>% html_text(),
                         date   = (html_data %>% html_nodes(".review-date")   %>% html_text())[-1:-2],
                         text   =  html_data %>% html_nodes(".review-text")   %>% html_text())
    
    # Bind page reviews together
    reviews <- reviews %>% bind_rows(review)
    # Pause the scraping for a random fraction of a second. Some websites block scraping if
    # the time between scrapes are the same.
    Sys.sleep(runif(1, 0, 1))
  }
  return(reviews)
}

# Function cleans reviews and converts into a Document Term Matrix
# Parameters: product and product_id from the Amazon url:
# reviews: a vectors of review text
# Return: Document Term Matrix
reviews_dtm <- function(reviews) {
  
  # First remove all unusual characters
  reviews <- gsub("[^[:graph:]]", " ", reviews)
  
  # Create corpus of reviews
  rcorpus <- Corpus(VectorSource(reviews))
  
  # Clean text in corpus
  rcorpus <- tm_map(rcorpus, removePunctuation)
  rcorpus <- tm_map(rcorpus, tolower)
  rcorpus <- tm_map(rcorpus, removeNumbers)
  rcorpus <- tm_map(rcorpus, removeWords, stopwords('english'))
  rcorpus <- tm_map(rcorpus, removeWords, c("apple", "iphone", "samsung", "phone")) # manual stopwords
  
  # Stem words, e.g. locked and locking become lock
  rcorpus <- tm_map(rcorpus, stemDocument) 
  
  # Strip unnecessary whitespaces
  rcorpus <- tm_map(rcorpus, stripWhitespace) 
  
  # convert to document term matrix
  dtm <- DocumentTermMatrix(rcorpus)
  
  return(dtm)
}

#####################################################################################
### Scraping Amazon reviews
#####################################################################################

# Specify whether data must be scraped or loaded
scrap_data <- FALSE # TRUE  if you want to scrape the most recent reviews
                    # FALSE if you want to use pre-scraped data (ver.2017-02-08)

if (scrap_data) {
  # Scraping iphone 7 reviews
  product        <- "Apple-iPhone-Unlocked-A1660-MNAC2LL"
  product_id     <- "B01LYT95XR"
  iphone_reviews <- amazon_scraping(product, product_id)
  
  # Scraping samsung EDGE s7 reviews
  product        <- "Samsung-Factory-Unlocked-International-Platinum"
  product_id     <- "B01CJU9MJI"
  s7edge_reviews <- amazon_scraping(product, product_id)
} else {
  load(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLIJG6bDR-8O.RData"))
}

#####################################################################################
### Text Mining
#####################################################################################

# Start with iphone7 reviews
dtm_iphone <- reviews_dtm(iphone_reviews$text)
dtm_s7edge <- reviews_dtm(s7edge_reviews$text)

# Determine frequency of each term in reviews
term_freq_iphone <- colSums(as.matrix(dtm_iphone))
term_freq_s7edge <- colSums(as.matrix(dtm_s7edge))

# Show high frequency terms
findFreqTerms(dtm_iphone, lowfreq = 30)
findFreqTerms(dtm_s7edge, lowfreq = 30)

# Wordcloud
df_freq_iphone <- data.frame(word = names(term_freq_iphone), freq = term_freq_iphone)
df_freq_s7edge <- data.frame(word = names(term_freq_s7edge), freq = term_freq_s7edge)
par(mfrow = c(1, 2))
wordcloud(words = df_freq_iphone$word, freq = df_freq_iphone$freq, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

wordcloud(words = df_freq_s7edge$word, freq = df_freq_s7edge$freq, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
par(mfrow = c(1, 1))

# Plot top 20 words
barplot(df_freq_iphone[order(-df_freq_iphone$freq),][1:20,]$freq, las = 2, 
        names.arg = df_freq_iphone[order(-df_freq_iphone$freq),][1:20,]$word,
        col ="lightblue", main ="Top 20 words: iPhone 7",
        ylab = "Frequency")

barplot(df_freq_s7edge[order(-df_freq_s7edge$freq),][1:20,]$freq, las = 2, 
        names.arg = df_freq_s7edge[order(-df_freq_s7edge$freq),][1:20,]$word,
        col ="pink", main ="Top 20 words: Galaxy S7 Edge",
        ylab = "Frequency")

# Find terms associated with like/wish
like_iphone <- findAssocs(dtm_iphone, c("like"), corlimit = 0.30)$like[1:20]
barplot(like_iphone, las = 2, 
        names.arg = names(like_iphone),
        col ="lightblue", main ="Associated words with 'Like': iPhone 7",
        ylab = "Relevancy")

wish_iphone <- findAssocs(dtm_iphone, c("wish"), corlimit = 0.30)$wish[1:20]
barplot(wish_iphone, las = 2, 
        names.arg = names(wish_iphone),
        col ="lightblue", main ="Associated words with 'Wish': iPhone 7",
        ylab = "Relevancy")

like_s7edge <- findAssocs(dtm_s7edge, c("like"), corlimit = 0.30)$like[1:20]
barplot(like_s7edge, las = 2, 
        names.arg = names(like_s7edge),
        col ="lightblue", main ="Associated words with 'Like': Galaxy S7 Edge",
        ylab = "Relevancy")

wish_s7edge <- findAssocs(dtm_s7edge, c("wish"), corlimit = 0.30)$wish[1:20]
barplot(wish_s7edge, las = 2, 
        names.arg = names(wish_s7edge),
        col ="lightblue", main ="Associated words with 'Wish': Galaxy S7 Edge",
        ylab = "Relevancy")

# Additional mining using "Drain"
drain_iphone <- findAssocs(dtm_iphone, c("drain"), corlimit = 0.60)$drain[1:20]
barplot(drain_iphone, las = 2, 
        names.arg = names(drain_iphone),
        col ="lightblue", main ="Associated words with 'Drain': iPhone 7",
        ylab = "Relevancy")

drain_s7edge <- findAssocs(dtm_s7edge, c("drain"), corlimit = 0.60)$drain[1:20]
barplot(drain_s7edge, las = 2, 
        names.arg = names(drain_s7edge),
        col ="lightblue", main ="Associated words with 'Drain': Galaxy S7 Edge",
        ylab = "Relevancy")

