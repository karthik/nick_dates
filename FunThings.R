library(lubridate)
library(dplyr)
toy <- read.csv("event_date_id_toyas.csv", stringsAsFactors = FALSE)
results <- toy %>%
           rowwise() %>%
            mutate(published_day = day_from_date(article_date),
               text_day  = return_days(toyas))

results <- results %>%
           mutate(guessing_date_in_text = day_in_text(article_date, published_day, text_day))


# These are the functions we use below


# -------------------------------------

day_from_date <- function(adate = NULL) {
  if(!is.null(adate)) {
    day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    which(grepl(weekdays(as.Date(adate,'%Y-%m-%d')),day_list))
  }
}

return_days <- function(text) {
  day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
 res <- as.numeric(which(sapply(day_list, grepl, text, ignore.case=TRUE)))
 if(length(res) == 0) {
   NULL
 } else {
   res[1]
 }
}

day_in_text <- function(article_day, aday, pdate) {
 date_diff <- aday - pdate
 if(date_diff > 0) {
   as.Date(article_day,'%Y-%m-%d') - date_diff
 } else {
   dd <- date_diff + 7
   as.Date(article_day,'%Y-%m-%d') - dd
 }
 }
