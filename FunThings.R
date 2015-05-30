toy <- read.csv("event_date_id_toyas.csv")


#adding test data columns
toy$test_article_day<-(c("Friday", "Tuesday", "Sunday"))
toy$test_tua_date<-(c("2011-12-6", "2011-11-7", "2011-11-2"))


#convert article_date to article day
toy$artday <- weekdays(as.Date(toy$article_date,'%Y-%m-%d'))

#extract eventday from TUA text
daynames<- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
dayregex<- paste0(daynames, collapse='|')


reg.out <- regexpr(dayregex, toy$toyas)
toy$toyaday <- substr(toy$toyas,reg.out,reg.out + attr(reg.out,"match.length")-1)

#identify number_of_days eventday occurred before article day
toy$dayspassed<-NULL

for (i in toy) {
  toy$dayspassed = which(daynames %in% toy$artday) - which(daynames %in% toy$toyaday)
}

for (i in toy) {
  toy$dayspassed = which(daynames %in% toy$artday) - which(daynames %in% toy$toyaday)
}


toy$d<-NULL
for (i in toy) {
  toy$d<-which(daynames %in% toy$artday)
}

(daynames%in%toy$artday)

e<- which(daynames %in% toy$toyaday)

####
#########
#################
##This is also broken!!!
#################
####
#########
number_of_days<- abs(which(daynames %in% (artday)) - grep(dayregex, toy$toyas, ignore.case=TRUE))

#it's recognizing that the differences between these things are 1, 3, and 4, but it does not have them in the right order:3, 1, 4.


#subtract number_of_days from article date to identify tuadate... and put that info in a column in the dataframe
toy$tua_date <- as.Date(toy$article_date,'%Y-%m-%d') - (dayspassed)


###!!!### if tua_date == NA, tua_date should equal article_date minus one day



############################
#Real Code
############################



#convert article date to article day
artday<-weekdays(as.Date(proscity$article_date,'%m-%d-%Y'))

#extract eventday from TUA text
daynames<- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
dayregex<- paste0(daynames, collapse='|')
daynames[grep(dayregex, proscity$tua[2], ignore.case=TRUE)]

#identify number_of_days eventday occurred before article day
which(daynames %in% "Sunday") - grep(dayregex, proscity$tua, ignore.case=TRUE)
number_of_days<- abs(which(daynames %in% artday) - grep(dayregex, proscity$tua, ignore.case=TRUE))

#subtract number_of_days from article date to identify eventdate... and put that info in a column in the dataframe

proscity$tua_date <- as.Date(proscity$article_date - number_of_days)
