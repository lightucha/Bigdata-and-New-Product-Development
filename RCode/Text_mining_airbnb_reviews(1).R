#####################################################################################
### Project : Airbnb Reviews
### Script : Text_mining_airbnb_reviews.R
### Description : Scraping Airbnb reviews and analyzing text
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
#   e.g. for https://www.amazon.com/Airbnb-Inc/dp/B00AJY6NGC/
#        product = Airbnb-Inc,
#        product_id = B00AJY6NGC
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
  rcorpus <- tm_map(rcorpus, removeWords, c("airbnb")) # manual stopwords
  
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
                    # FALSE if you want to use pre-scraped data (ver.2017-03-30)

if (scrap_data) {
  # Scraping Airbnb reviews
  product        <- "Airbnb-Inc"
  product_id     <- "B00AJY6NGC"
  airbnb_reviews <- amazon_scraping(product, product_id)
} else {
  load(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLINGqbCRecL.RData"))
}

#####################################################################################
### Text Mining
#####################################################################################

# Pre-processing with airbnb reviews
dtm_airbnb <- reviews_dtm(airbnb_reviews$text)

# Determine frequency of each term in reviews
term_freq_airbnb <- colSums(as.matrix(dtm_airbnb))

# Show high frequency terms
findFreqTerms(dtm_airbnb, lowfreq = 30)

# Wordcloud
df_freq_airbnb <- data.frame(word = names(term_freq_airbnb), freq = term_freq_airbnb)
wordcloud(words = df_freq_airbnb$word, freq = df_freq_airbnb$freq, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# Plot top 20 words
barplot(df_freq_airbnb[order(-df_freq_airbnb$freq),][1:20,]$freq, las = 2, 
        names.arg = df_freq_airbnb[order(-df_freq_airbnb$freq),][1:20,]$word,
        col ="lightblue", main ="Top 20 words: Airbnb",
        ylab = "Frequency")

# Find terms associated with like/wish
like_airbnb <- findAssocs(dtm_airbnb, c("like"), corlimit = 0.30)$like[1:20]
barplot(like_airbnb, las = 2, 
        names.arg = names(like_airbnb),
        col ="lightblue", main ="Associated words with 'Like': Airbnb",
        ylab = "Relevancy")

wish_airbnb <- findAssocs(dtm_airbnb, c("wish"), corlimit = 0.30)$wish[1:20]
barplot(wish_airbnb, las = 2, 
        names.arg = names(wish_airbnb),
        col ="lightblue", main ="Associated words with 'Wish': Airbnb",
        ylab = "Relevancy")

# Find interesting stories and share with the class!
