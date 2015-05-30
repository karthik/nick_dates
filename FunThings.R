library(lubridate)
library(dplyr)
toy <- read.csv("event_date_id_toyas.csv", stringsAsFactors = FALSE)
results <- toy %>%
           rowwise() %>%
            mutate(published_day = day_from_date(article_date),
               text_day  = return_days(toyas))

results <- results %>%
           mutate(guessing_date_in_text = day_in_text(article_date, text_day))


# These are the functions we use below


# -------------------------------------

day_from_date <- function(adate = NULL) {
  if(!is.null(adate)) {
    weekdays(as.Date(adate,'%Y-%m-%d'))
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

day_in_text <- function(aday, pdate) {
  day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
 a1 <- which(grepl(weekdays(as.Date(adate,'%Y-%m-%d')), day_list))
 a2 <- pdate
 date_diff <- a1 - a2
 if(date_diff > 0) {
   as.Date(adate,'%Y-%m-%d') - date_diff
 } else {
   as.Date(adate,'%Y-%m-%d') - (date_diff + 7)
 }
 }
