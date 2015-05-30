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
   dd <- ifelse(date_diff!=0, date_diff + 7, 0)
   as.Date(article_day,'%Y-%m-%d') - dd
 }
 }
