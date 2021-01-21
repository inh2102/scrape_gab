library(RSelenium)
library(rvest)
library(tidyverse)

start_selenium <- function() {
  url <- 'https://gab.com/groups'

  remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
  )
  remDr$open(silent=TRUE)
  cat("Navigating to Gab and waiting for CloudFlare, please wait...")
  remDr$navigate(url)
  Sys.sleep(20)
  cat("\nGrabbing page contents...")

  webElem <- remDr$findElement(using = "xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "_2dPcT", " " ))]')
  webElem$clickElement()
  user <- 'inh2102'
  pass <- 'abc123jack'
  Sys.sleep(5)
  username <- remDr$findElement(using = "id", value = "user_email")
  username$sendKeysToElement(list(user))
  password <- remDr$findElement(using = "id", value = "user_password")
  password$sendKeysToElement(list(pass))
  nextButton <- remDr$findElement(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "btn", " " ))]')
  nextButton$clickElement()
  Sys.sleep(5)

  groups <- remDr$findElement(using = "xpath", value = '//*[contains(concat( " ", @class, " " ), concat( " ", "L4pn5", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]')
  groups$clickElement()
  Sys.sleep(5)
}

start_selenium()

scrape_gab <- function(scrolls=100) {
  df <- data.frame(author=NA,username=NA,pub_date=NA,text=NA,pull_time=NA)
  df_temp <- NULL
  i <- 0

  while (i <= scrolls) {
  
    page_source <- remDr$getPageSource()
    
    author <- read_html(page_source[[1]]) %>% html_nodes('strong._3_54N') %>% html_text()
    
    username <- read_html(page_source[[1]]) %>% html_nodes("._33mR1._3hcKE") %>%
      html_text() 
    
    pub_date <- read_html(page_source[[1]]) %>% html_nodes("time") %>%
      html_text()
    
    text <- read_html(page_source[[1]]) %>% html_nodes("._1FwZr") %>%
      html_text() %>% 
      str_replace_all("\r?\n|\r", " ") %>% 
      gsub('[\r\n\t]', '', .) %>% 
      str_replace_all("\\s{2,}"," ") %>%
      gsub('\"', "", ., fixed = TRUE) %>%
      gsub("\\\\", "",.) %>%
      rtweet::plain_tweets() # to ascii
    
    df_temp <- cbind(author=author,username=username,pub_date=pub_date,text=text,pull_time=Sys.time()) %>% as.data.frame()
    df <- rbind(df,df_temp)
    
    remDr$executeScript(paste("scroll(0,",i*10000,");"))
    Sys.sleep(3)
    if (i %% 5 == 0) { cat("\n",i,"/",scrolls,"complete") }
    i <- i+1
  }

df <- df %>% tibble() %>%
  mutate(pull_time = lubridate::as_datetime(as.numeric(pull_time))) %>%
  filter(!is.na(df)&!duplicated(text))
}

suppressWarnings(scrape_gab()) 